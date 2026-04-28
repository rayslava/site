(in-package :site.tests)

(in-suite all-tests)

(defmacro with-memory-storage (&body body)
  "Bind site.storage:*backend* to a fresh memory-backend for BODY."
  `(let ((site.storage:*backend* (site.storage:make-backend :memory)))
     ,@body))

(defun post-url (id)
  (format nil "https://rayslava.com/blog?id=~A" id))

(defun store-like (event-id post-id)
  "Persist a Like event through the facade, mirroring what blog-inbox does."
  (site.storage:event-save
   (list :id event-id :object-id (post-url post-id) :published 0
         :event-type "Like"
         :event (format nil "{\"id\":\"~A\",\"type\":\"Like\"}" event-id))))

(defun store-announce (event-id post-id)
  (site.storage:event-save
   (list :id event-id :object-id (post-url post-id) :published 0
         :event-type "Announce"
         :event (format nil "{\"id\":\"~A\",\"type\":\"Announce\"}" event-id))))

(defun store-reply (event-id in-reply-to-post-id
                    &key (actor "https://example.com/users/alice")
                      (content "hello")
                      (url "https://example.com/notes/42")
                      (published "2024-01-01T12:00:00Z")
                      (public t))
  "Persist a Create reply event. The :event JSON payload mirrors a
Mastodon inbox POST body so direct-replies can decode it."
  (let* ((to (if public
                 "[\"https://www.w3.org/ns/activitystreams#Public\"]"
                 "[]"))
         (cc "[]")
         (body (format nil
                       "{\"id\":\"~A\",\"type\":\"Create\",\"object\":{\"attributedTo\":\"~A\",\"url\":\"~A\",\"published\":\"~A\",\"content\":\"~A\",\"to\":~A,\"cc\":~A}}"
                       event-id actor url published content to cc)))
    (site.storage:event-save
     (list :id event-id :object-id event-id :published 0
           :event-type "Create"
           :event body
           :reply-to (post-url in-reply-to-post-id)))))

(test activitypub-reactions-number-counts-by-type
  "reactions-number sums Like vs Announce independently for a given post."
  (with-memory-storage
    (store-like "e1" 42)
    (store-like "e2" 42)
    (store-like "e3" 42)
    (store-announce "e4" 42)
    (store-like "e5" 99) ; different post
    (is (= 3 (site.activitypub:reactions-number 42 "Like")))
    (is (= 1 (site.activitypub:reactions-number 42 "Announce")))
    (is (= 1 (site.activitypub:reactions-number 99 "Like")))
    (is (= 0 (site.activitypub:reactions-number 999 "Like")))))

(test activitypub-dislike-removes-event
  "Calling dislike with a Like id deletes the row; subsequent count drops."
  (with-memory-storage
    (store-like "e1" 7)
    (store-like "e2" 7)
    (is (= 2 (site.activitypub:reactions-number 7 "Like")))
    (site.activitypub::dislike "e1")
    (is (= 1 (site.activitypub:reactions-number 7 "Like")))))

(test activitypub-direct-replies-decodes-fields
  "direct-replies decodes the stored Create event and exposes the
ActivityStreams fields the blog renderer needs."
  (with-memory-storage
    (store-reply "note-1" 100
                 :actor "https://mastodon.example/@bob"
                 :url "https://mastodon.example/@bob/111"
                 :content "first!"
                 :published "2024-06-15T10:00:00Z")
    (let* ((replies (site.activitypub:direct-replies 100))
           (reply (first replies)))
      (is (= 1 (length replies)))
      (is (equal "https://mastodon.example/@bob"
                 (cdr (assoc :actor reply))))
      (is (equal "first!" (cdr (assoc :content reply))))
      (is (equal "https://mastodon.example/@bob/111"
                 (cdr (assoc :url reply))))
      (is-true (cdr (assoc :public reply))))))

(test activitypub-direct-replies-marks-non-public
  "A reply without the activitystreams#Public recipient is :public NIL."
  (with-memory-storage
    (store-reply "priv" 100 :public nil)
    (let ((reply (first (site.activitypub:direct-replies 100))))
      (is-false (cdr (assoc :public reply))))))

(test activitypub-direct-replies-empty
  "No events → empty list, not NIL-vs-empty confusion."
  (with-memory-storage
    (is (null (site.activitypub:direct-replies 100)))))

(test activitypub-direct-replies-filters-by-post-id
  "Replies to a different post are not returned."
  (with-memory-storage
    (store-reply "r1" 100)
    (store-reply "r2" 200)
    (is (= 1 (length (site.activitypub:direct-replies 100))))
    (is (= 1 (length (site.activitypub:direct-replies 200))))))

(test activitypub-flatten-replies-handles-empty
  "flatten-replies on an empty list yields an empty list."
  (is (null (site.activitypub:flatten-replies nil))))
