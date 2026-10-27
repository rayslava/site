(in-package :site.tests)

(in-suite all-tests)

(test ap-delivery-generate-signed-header-has-shape
  "generate-signed-header produces a Signature header value that declares
the right algorithm, the required headers list, and a base64 signature."
  ;; generate-signed-header uses site.config:*activitypub-private-key*,
  ;; which is NIL unless keys are loaded. Generate an ephemeral keypair
  ;; for the duration of the test.
  (let ((saved-priv site.config:*activitypub-private-key*))
    (unwind-protect
         (progn
           (multiple-value-bind (priv pub) (generate-rsa-keypair)
             (declare (ignore pub))
             (setf site.config:*activitypub-private-key* priv))
           (let ((hdr (site.activitypub:generate-signed-header
                       "https://rayslava.com/ap/actor/blog#main-key"
                       "/users/alice/inbox"
                       "alice.example"
                       "Wed, 01 Jun 2024 12:00:00 GMT"
                       "SHA-256=abc")))
             (is-true (search "algorithm=\"rsa-sha256\"" hdr))
             (is-true (search "headers=\"(request-target) host date digest\"" hdr))
             (is-true (search "signature=\"" hdr))
             (is-true (search "keyId=\"https://rayslava.com/ap/actor/blog#main-key\"" hdr))))
      (setf site.config:*activitypub-private-key* saved-priv))))

(defmacro with-captured-http (binding &body body)
  "Bind BINDING to a list where each outbound send-signed call is recorded
as (:url U :headers H :content C). Also silences hunchentoot:log-message*
since send-signed hits it on HTTP errors; without a live acceptor that
would unbind-variable."
  `(let* ((,binding nil)
          (site.activitypub:*http-send-fn*
            (lambda (url &key headers content)
              (push (list :url url :headers headers :content content) ,binding)
              "{}"))
          (orig-log (symbol-function 'hunchentoot:log-message*)))
     (unwind-protect
          (progn
            (setf (symbol-function 'hunchentoot:log-message*)
                  #'silent-log-message*)
            ,@body
            (setf ,binding (nreverse ,binding)))
       (setf (symbol-function 'hunchentoot:log-message*) orig-log))))

(test ap-delivery-send-signed-makes-one-post
  "send-signed invokes *http-send-fn* exactly once with a URL ending in
/inbox and a Signature header."
  (let ((saved-priv site.config:*activitypub-private-key*))
    (unwind-protect
         (progn
           (multiple-value-bind (priv pub) (generate-rsa-keypair)
             (declare (ignore pub))
             (setf site.config:*activitypub-private-key* priv))
           (with-captured-http calls
             (site.activitypub:send-signed
              "https://alice.example/users/alice"
              "{}")
             (is (= 1 (length calls)))
             (is-true (search "/inbox" (getf (first calls) :url)))
             (let ((hdrs (getf (first calls) :headers)))
               (is-true (assoc "Signature" hdrs :test #'string-equal))
               (is-true (assoc "digest" hdrs :test #'string-equal))
               (is-true (assoc "date" hdrs :test #'string-equal)))))
      (setf site.config:*activitypub-private-key* saved-priv))))

(test ap-delivery-maybe-deliver-new-post-updates-lastpost
  "maybe-deliver-new-post walks every subscriber with lastpost < post-id,
posts to them, and advances their lastpost to the post's id."
  (let ((site.storage:*backend* (site.storage:make-backend :memory))
        (saved-priv site.config:*activitypub-private-key*))
    (unwind-protect
         (progn
           (multiple-value-bind (priv pub) (generate-rsa-keypair)
             (declare (ignore pub))
             (setf site.config:*activitypub-private-key* priv))
           (site.storage:subscriber-save "https://a.example/actor" nil)
           (site.storage:subscriber-save "https://b.example/actor" nil)
           (let ((post (make-instance 'site.blog-post:blog-post
                                      :id 3700000123
                                      :subject "fedi post"
                                      :tags '("fedi")
                                      :post (lambda () "<p>body</p>"))))
             (with-captured-http calls
               (site.activitypub::maybe-deliver-new-post post)
               (is (= 2 (length calls)))
               (is (= 3700000123
                      (getf (site.storage:subscriber-find "https://a.example/actor")
                            :lastpost)))
               (is (= 3700000123
                      (getf (site.storage:subscriber-find "https://b.example/actor")
                            :lastpost))))))
      (setf site.config:*activitypub-private-key* saved-priv))))

(test ap-delivery-maybe-update-post-reaches-notified
  "maybe-update-post POSTs to subscribers whose lastpost >= post-id."
  (let ((site.storage:*backend* (site.storage:make-backend :memory))
        (saved-priv site.config:*activitypub-private-key*))
    (unwind-protect
         (progn
           (multiple-value-bind (priv pub) (generate-rsa-keypair)
             (declare (ignore pub))
             (setf site.config:*activitypub-private-key* priv))
           (site.storage:subscriber-save "https://a.example/actor" nil)
           (site.storage:subscriber-update-lastpost "https://a.example/actor" 5000)
           (let ((post (make-instance 'site.blog-post:blog-post
                                      :id 100  ; smaller than lastpost 5000
                                      :subject "update"
                                      :tags '("fedi")
                                      :post (lambda () "<p>body</p>"))))
             (with-captured-http calls
               (site.activitypub:maybe-update-post post)
               (is (= 1 (length calls))))))
      (setf site.config:*activitypub-private-key* saved-priv))))

(defun join-delivery-threads ()
  "Wait for every background delivery thread to finish."
  (dolist (thread (bt:all-threads))
    (when (equal "ap-delivery" (bt:thread-name thread))
      (bt:join-thread thread))))

(test ap-delivery-in-background-uses-caller-context
  "deliver-in-background runs FN in another thread that still sees the
caller's storage backend and HTTP sender."
  (let ((site.storage:*backend* (site.storage:make-backend :memory))
        (saved-priv site.config:*activitypub-private-key*))
    (unwind-protect
         (progn
           (multiple-value-bind (priv pub) (generate-rsa-keypair)
             (declare (ignore pub))
             (setf site.config:*activitypub-private-key* priv))
           (site.storage:subscriber-save "https://a.example/actor" nil)
           (let ((post (make-instance 'site.blog-post:blog-post
                                      :id 3700000123
                                      :subject "fedi post"
                                      :tags '("fedi")
                                      :post (lambda () "<p>body</p>"))))
             (with-captured-http calls
               (let ((thread (site.activitypub:deliver-in-background
                              (lambda ()
                                (site.activitypub:maybe-deliver-new-posts (list post))))))
                 (is (not (eq thread (bt:current-thread))))
                 (bt:join-thread thread))
               (is (= 1 (length calls)))
               (is (= 3700000123
                      (getf (site.storage:subscriber-find "https://a.example/actor")
                            :lastpost))))))
      (setf site.config:*activitypub-private-key* saved-priv))))

(test ap-delivery-in-background-survives-errors
  "An error inside the background delivery is logged, not raised."
  (with-captured-http calls
    (let ((thread (site.activitypub:deliver-in-background
                   (lambda () (error "remote server exploded")))))
      (finishes (bt:join-thread thread)))
    (is (null calls))))

(test ap-inbox-follow-delivers-backlog-in-background
  "A Follow sends the Accept inline and the backlog from a background thread."
  (with-quiet-http-and-memory
    (site.blog-registry:register-post
     (make-instance 'site.blog-post:blog-post
                    :id 3700000124
                    :subject "fedi post"
                    :tags '("fedi")
                    :post (lambda () "<p>body</p>")))
    (with-captured-http calls
      (site.activitypub:handle-inbox-activity
       (follow-activity "https://alice.example/actor") "{}")
      (join-delivery-threads)
      ;; Accept, then the post
      (is (= 2 (length calls))))))

(test ap-delivery-signed-get-headers-verify
  "signed-get-headers signs (request-target) host date with the site's
key, in a form the inbox verifier (and Mastodon) can check."
  (let ((saved-priv site.config:*activitypub-private-key*))
    (unwind-protect
         (multiple-value-bind (priv pub) (generate-rsa-keypair)
           (setf site.config:*activitypub-private-key* priv)
           (dolist (case '(("https://social.example/users/alice#main-key"
                            "social.example" "get /users/alice")
                           ("https://social.example:8443/actor?id=1"
                            "social.example:8443" "get /actor?id=1")))
             (destructuring-bind (url host target) case
               (let* ((headers (site.activitypub:signed-get-headers url))
                      (sig (site.ap-signature:parse-signature-header
                            (cdr (assoc "signature" headers :test #'string=))))
                      (message (format nil "(request-target): ~A~%host: ~A~%date: ~A"
                                       target host
                                       (cdr (assoc "date" headers :test #'string=)))))
                 (is (equal host (cdr (assoc "host" headers :test #'string=))))
                 (is (equal "https://rayslava.com/ap/actor/blog#main-key"
                            (site.ap-signature:signature-keyid sig)))
                 (is (equal "(request-target) host date"
                            (site.ap-signature:signature-headers sig)))
                 (is-true (site.crypto:rsassa-pkcs1-v1_5-verify
                           pub
                           (ironclad:ascii-string-to-byte-array message)
                           (site.ap-signature:signature-bytes sig)
                           :sha256))))))
      (setf site.config:*activitypub-private-key* saved-priv))))
