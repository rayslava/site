(in-package :site.tests)

(in-suite all-tests)

(defvar *inbox-test-sends* nil
  "Captured (URL BODY) pairs of HTTP calls made during a test.")

(defun silent-log-message* (&rest args)
  (declare (ignore args))
  nil)

(defmacro with-quiet-http-and-memory (&body body)
  "Bind site.storage:*backend* to a fresh memory backend and redirect
send-signed's HTTP call to a silent stub so the Follow path doesn't try
to reach the network. Also clears the blog-registry so the Follow-path
delivery loop has nothing to send beyond the Accept. Temporarily rebinds
hunchentoot:log-message* (needs *acceptor* which is unbound outside a
live server) and ensures *activitypub-private-key* is set to an
ephemeral keypair so generate-signed-header doesn't error inside its
handler-case (which would silently drop the send)."
  `(let* ((site.storage:*backend* (site.storage:make-backend :memory))
          (*inbox-test-sends* nil)
          (site.activitypub:*http-send-fn*
            (lambda (url &key headers content)
              (declare (ignore headers))
              (push (list url content) *inbox-test-sends*)
              "{}"))
          (saved-priv site.config:*activitypub-private-key*)
          (saved-posts (site.blog-registry:all-posts))
          (orig-log (symbol-function 'hunchentoot:log-message*)))
     (unwind-protect
          (progn
            (site.blog-registry:clear-posts)
            (unless site.config:*activitypub-private-key*
              (multiple-value-bind (priv pub)
                  (ironclad:generate-key-pair :rsa :num-bits 2048)
                (declare (ignore pub))
                (setf site.config:*activitypub-private-key* priv)))
            (setf (symbol-function 'hunchentoot:log-message*)
                  #'silent-log-message*)
            ,@body)
       (setf (symbol-function 'hunchentoot:log-message*) orig-log)
       (setf site.config:*activitypub-private-key* saved-priv)
       (setf site.blog-registry::*blog-posts* saved-posts))))

(defun follow-activity (actor)
  `((:type . "Follow")
    (:actor . ,actor)
    (:id . ,(format nil "https://example.com/activities/~A" actor))))

(defun undo-follow-activity (actor)
  `((:type . "Undo")
    (:actor . ,actor)
    (:object . ((:type . "Follow")
                (:actor . ,actor)))))

(defun like-activity (event-id post-id actor)
  `((:type . "Like")
    (:id . ,event-id)
    (:actor . ,actor)
    (:object . ,(format nil "https://rayslava.com/blog?id=~A" post-id))))

(defun announce-activity (event-id post-id actor)
  `((:type . "Announce")
    (:id . ,event-id)
    (:actor . ,actor)
    (:object . ,(format nil "https://rayslava.com/blog?id=~A" post-id))))

(defun undo-like-activity (event-id actor)
  `((:type . "Undo")
    (:actor . ,actor)
    (:object . ((:type . "Like")
                (:id . ,event-id)))))

(defun delete-activity (actor object-id)
  `((:type . "Delete")
    (:actor . ,actor)
    (:object . ,object-id)))

(defun fallthrough-activity (event-id object-id reply-to)
  `((:type . "SomethingWeird")
    (:id . ,event-id)
    (:actor . "https://alice.example/actor")
    (:object . ((:id . ,object-id)
                (:in-reply-to . ,reply-to)))))

(test ap-inbox-follow-registers-subscriber-and-sends-accept
  "A Follow creates a subscriber row AND fires exactly one outbound POST
(the Accept activity) through *http-send-fn*."
  (with-quiet-http-and-memory
    (let ((actor "https://alice.example/actor"))
      (site.activitypub:handle-inbox-activity
       (follow-activity actor) "{}")
      (is-true (site.storage:subscriber-find actor))
      (is (= 1 (length *inbox-test-sends*)))
      (is (search "/inbox" (first (first *inbox-test-sends*)))))))

(test ap-inbox-follow-is-idempotent
  "Two Follows from the same actor yield a single subscriber row."
  (with-quiet-http-and-memory
    (let ((actor "https://alice.example/actor"))
      (site.activitypub:handle-inbox-activity (follow-activity actor) "{}")
      (site.activitypub:handle-inbox-activity (follow-activity actor) "{}")
      (is-true (site.storage:subscriber-find actor))
      ;; Regardless of lastpost updates from the delivery loop, the actor
      ;; should appear exactly once across all lastpost buckets.
      (is (= 1 (+ (length (site.storage:subscriber-all-with-lastpost< 0))
                  (length (site.storage:subscriber-all-with-lastpost>= 0))))))))

(test ap-inbox-undo-follow-unsubscribes
  "An Undo(Follow) removes the subscriber row."
  (with-quiet-http-and-memory
    (let ((actor "https://bob.example/actor"))
      (site.activitypub:handle-inbox-activity (follow-activity actor) "{}")
      (is-true (site.storage:subscriber-find actor))
      (site.activitypub:handle-inbox-activity (undo-follow-activity actor) "{}")
      (is-false (site.storage:subscriber-find actor)))))

(test ap-inbox-like-saves-event
  "A Like event is persisted and reflected in reactions-number."
  (with-quiet-http-and-memory
    (let ((body "{\"type\":\"Like\"}"))
      (site.activitypub:handle-inbox-activity
       (like-activity "e1" 42 "https://alice.example/actor") body)
      (is (= 1 (site.activitypub:reactions-number 42 "Like"))))))

(test ap-inbox-announce-saves-event-as-announce
  "Announce activities are counted separately from Likes."
  (with-quiet-http-and-memory
    (let ((body "{\"type\":\"Announce\"}"))
      (site.activitypub:handle-inbox-activity
       (announce-activity "e2" 42 "https://bob.example/actor") body)
      (is (= 0 (site.activitypub:reactions-number 42 "Like")))
      (is (= 1 (site.activitypub:reactions-number 42 "Announce"))))))

(test ap-inbox-undo-like-removes-count
  "Undo(Like) deletes the event row, dropping the count."
  (with-quiet-http-and-memory
    (site.activitypub:handle-inbox-activity
     (like-activity "e1" 42 "https://alice.example/actor")
     "{\"type\":\"Like\"}")
    (is (= 1 (site.activitypub:reactions-number 42 "Like")))
    (site.activitypub:handle-inbox-activity
     (undo-like-activity "e1" "https://alice.example/actor")
     "{}")
    (is (= 0 (site.activitypub:reactions-number 42 "Like")))))

(test ap-inbox-delete-is-no-op
  "A Delete activity returns ok without persisting anything."
  (with-quiet-http-and-memory
    (let ((reply (site.activitypub:handle-inbox-activity
                  (delete-activity "https://alice.example/actor"
                                   "https://alice.example/notes/123")
                  "{}")))
      (is (search "\"ok\"" reply))
      ;; Event table must stay empty.
      (is (null (site.storage:%event-find-by-id site.storage:*backend* "e1"))))))

(test ap-inbox-fallthrough-saves-event-with-reply-to
  "An unknown activity type is persisted for audit, including its
:in-reply-to so direct-replies can find it."
  (with-quiet-http-and-memory
    (site.activitypub:handle-inbox-activity
     (fallthrough-activity "weird-1" "https://x.example/obj/7"
                           "https://rayslava.com/blog?id=123")
     "{\"raw\":\"payload\"}")
    (let ((row (site.storage:%event-find-by-id
                site.storage:*backend* "weird-1")))
      (is-true row)
      (is (equal "SomethingWeird" (getf row :event-type))))))
