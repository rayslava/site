(in-package :site.tests)

(in-suite all-tests)

;;;; Real HTTP integration tests. Start a live hunchentoot acceptor on
;;;; an ephemeral port, issue dex:get / dex:post against it, and assert
;;;; status, Content-Type, and body markers. These are the tests that
;;;; catch cases the unit tests can't — handler body returning NIL,
;;;; content-type missing, URI routing typos, HTTP method mismatches.

(defvar *test-acceptor* nil)
(defvar *test-port* nil)

(defun int-test-pem ()
  "PEM string used as the publicKeyPem in the actor JSON under test.
A minimal but validly shaped 2048-bit key (the same fixture used by the
PEM parser tests)."
  *sample-actor-pem*)

(defun int-test-start-server ()
  "Start hunchentoot on an ephemeral port with a memory backend,
an in-memory RSA keypair, and a cleared blog registry. Returns the
port. Safe to call repeatedly — stops any existing acceptor first."
  (when *test-acceptor*
    (hunchentoot:stop *test-acceptor*)
    (setf *test-acceptor* nil))
  (setf site.storage:*backend* (site.storage:make-backend :memory))
  (site.blog-registry:clear-posts)
  (unless site.config:*activitypub-private-key*
    (multiple-value-bind (priv pub)
        (ironclad:generate-key-pair :rsa :num-bits 2048)
      (setf site.config:*activitypub-private-key* priv
            site.config:*activitypub-public-key* pub)))
  (unless site.config:*activitypub-public-key-pem*
    (setf site.config:*activitypub-public-key-pem* (int-test-pem)))
  ;; Silence hunchentoot's access/message logging through a null stream.
  (let* ((null-stream (make-broadcast-stream))
         (acceptor (make-instance 'hunchentoot:easy-acceptor
                                  :port 0
                                  :access-log-destination null-stream
                                  :message-log-destination null-stream)))
    (hunchentoot:start acceptor)
    (setf *test-acceptor* acceptor)
    (setf *test-port* (hunchentoot:acceptor-port acceptor))
    *test-port*))

(defun int-test-stop-server ()
  (when *test-acceptor*
    (hunchentoot:stop *test-acceptor*)
    (setf *test-acceptor* nil
          *test-port* nil)))

(defmacro with-integration-server (&body body)
  `(let ((saved-priv site.config:*activitypub-private-key*)
         (saved-pub site.config:*activitypub-public-key*)
         (saved-pem site.config:*activitypub-public-key-pem*)
         (saved-posts (site.blog-registry:all-posts))
         (orig-log (symbol-function 'hunchentoot:log-message*)))
     (unwind-protect
          (progn
            (setf (symbol-function 'hunchentoot:log-message*)
                  #'silent-log-message*)
            (int-test-start-server)
            ,@body)
       (setf (symbol-function 'hunchentoot:log-message*) orig-log)
       (int-test-stop-server)
       (setf site.config:*activitypub-private-key* saved-priv
             site.config:*activitypub-public-key* saved-pub
             site.config:*activitypub-public-key-pem* saved-pem)
       (setf site.blog-registry::*blog-posts* saved-posts))))

(defun url (path)
  (format nil "http://localhost:~A~A" *test-port* path))

(defun int-get (path &key (accept nil))
  "GET PATH. Return (values body status content-type). dex signals on
non-2xx — we capture the response via handler-case so 404s don't kill
the test."
  (handler-case
      (multiple-value-bind (body status headers)
          (dex:get (url path)
                   :headers (when accept `(("accept" . ,accept)))
                   :force-string t
                   :verbose nil)
        (values body status (gethash "content-type" headers)))
    (dex:http-request-failed (e)
      (values (dex:response-body e)
              (dex:response-status e)
              (gethash "content-type" (dex:response-headers e))))))

(defun int-post (path content &key (content-type "application/json"))
  (handler-case
      (multiple-value-bind (body status headers)
          (dex:post (url path)
                    :headers `(("content-type" . ,content-type))
                    :content content
                    :force-string t
                    :verbose nil)
        (values body status (gethash "content-type" headers)))
    (dex:http-request-failed (e)
      (values (dex:response-body e)
              (dex:response-status e)
              (gethash "content-type" (dex:response-headers e))))))

;;;; Content-Type assertions are tight because ActivityPub consumers
;;;; (Mastodon, Pleroma, Misskey) reject peers whose actor endpoint does
;;;; not advertise application/activity+json, and reject POSTed activities
;;;; unless they carry application/ld+json with the activitystreams
;;;; profile parameter. We also check RFC 6415 / RFC 7033 for webfinger.

(defun ct-content-type-is (expected-media-type ct)
  "T iff CT's media type equals EXPECTED-MEDIA-TYPE exactly (case-insensitive),
ignoring any parameters after the first ';'."
  (when ct
    (let ((semi (position #\; ct)))
      (string-equal expected-media-type
                    (string-trim '(#\Space) (subseq ct 0 semi))))))

(defun ct-has-as-profile-p (ct)
  "T iff CT carries the ActivityStreams profile parameter. The spec
allows either http:// or https:// form of the profile URI."
  (when ct
    (or (search "profile=\"https://www.w3.org/ns/activitystreams\"" ct)
        (search "profile=\"http://www.w3.org/ns/activitystreams\"" ct))))

(test int-actor-blog-returns-activity-json
  "GET /ap/actor/blog returns 200, advertises application/activity+json
exactly, and decodes to a Person actor with a publicKey."
  (with-integration-server
    (multiple-value-bind (body status ct) (int-get "/ap/actor/blog")
      (is (= 200 status))
      (is-true (ct-content-type-is "application/activity+json" ct)
               "Content-Type media type must be application/activity+json (got ~S)" ct)
      (let ((decoded (cl-json:decode-json-from-string body)))
        (is (equal "Person" (cdr (assoc :type decoded))))
        (is (equal "https://rayslava.com/ap/actor/blog"
                   (cdr (assoc :id decoded))))
        (is-true (assoc :public-key decoded))))))

(test int-webfinger-returns-jrd-json
  "GET /.well-known/webfinger returns 200 with Content-Type
application/jrd+json (RFC 7033)."
  (with-integration-server
    (multiple-value-bind (body status ct)
        (int-get "/.well-known/webfinger?resource=acct:blog@rayslava.com")
      (is (= 200 status))
      (is-true (ct-content-type-is "application/jrd+json" ct)
               "webfinger Content-Type must be application/jrd+json (got ~S)" ct)
      (let ((decoded (cl-json:decode-json-from-string body)))
        (is (equal "acct:blog@rayslava.com" (cdr (assoc :subject decoded))))))))

(test int-masto-lookup-returns-json
  "GET /api/v1/accounts/lookup returns 200 with application/json.
Mastodon's REST API contract is application/json, not jrd+json."
  (with-integration-server
    (multiple-value-bind (body status ct)
        (int-get "/api/v1/accounts/lookup/?acct=blog")
      (is (= 200 status))
      (is-true (ct-content-type-is "application/json" ct)
               "masto lookup Content-Type must be application/json (got ~S)" ct)
      (let ((decoded (cl-json:decode-json-from-string body)))
        (is (equal "rayslava" (cdr (assoc :username decoded))))))))

(test int-outbox-advertises-activitystreams-profile
  "GET /ap/actor/blog/outbox returns 200 with application/ld+json AND
the activitystreams profile parameter (required by the AP spec)."
  (with-integration-server
    (multiple-value-bind (body status ct)
        (int-get "/ap/actor/blog/outbox")
      (is (= 200 status))
      (is-true (ct-content-type-is "application/ld+json" ct)
               "outbox media type must be application/ld+json (got ~S)" ct)
      (is-true (ct-has-as-profile-p ct)
               "outbox Content-Type must carry the activitystreams profile (got ~S)" ct)
      (let ((decoded (cl-json:decode-json-from-string body)))
        (is (equal "OrderedCollection" (cdr (assoc :type decoded))))
        (is (= 0 (cdr (assoc :total-items decoded))))))))

(test int-outbox-post-not-found
  "/ap/actor/blog/outbox/post?id=999 returns 404 with a JSON error body."
  (with-integration-server
    (multiple-value-bind (body status ct)
        (int-get "/ap/actor/blog/outbox/post?id=999")
      (is (= 404 status))
      (is-true (ct-content-type-is "application/ld+json" ct)
               "404 body should still be JSON-typed so federation peers can parse it (got ~S)" ct)
      (is-true (search "Post not found" body)))))

(test int-outbox-post-returns-fedi-post
  "When a fedi-tagged post exists, /ap/actor/blog/outbox/post?id=X
returns a JSON Create activity with the activitystreams profile."
  (with-integration-server
    (site.blog-registry:register-post
     (make-instance 'site.blog-post:blog-post
                    :id 3700000000
                    :subject "hello"
                    :tags '("en" "fedi")
                    :post (lambda () "<p>body</p>")))
    (multiple-value-bind (body status ct)
        (int-get "/ap/actor/blog/outbox/post?id=3700000000")
      (is (= 200 status))
      (is-true (ct-content-type-is "application/ld+json" ct)
               "outbox-post media type must be application/ld+json (got ~S)" ct)
      (is-true (ct-has-as-profile-p ct)
               "outbox-post must carry the activitystreams profile (got ~S)" ct)
      (let ((decoded (cl-json:decode-json-from-string body)))
        (is (equal "Create" (cdr (assoc :type decoded))))))))

(test int-rss-returns-xml
  "/rss returns an RSS 2.0 feed: XML media type, well-formed XML over
the wire (the ultimate test — if cxml can't parse this, no feed reader
will), and the post body present in CDATA form."
  (with-integration-server
    (site.blog-registry:register-post
     (make-instance 'site.blog-post:blog-post
                    :id 3700000000
                    :subject "first & only"
                    :tags '("en")
                    :post (lambda () "<p>line<br>more</p>")))
    (multiple-value-bind (body status ct) (int-get "/rss")
      (is (= 200 status))
      (is-true (or (ct-content-type-is "application/rss+xml" ct)
                   (ct-content-type-is "application/xml" ct)
                   (ct-content-type-is "text/xml" ct))
               "RSS feed must advertise an XML media type (got ~S)" ct)
      (is-true (search "<?xml version=\"1.0\"" body))
      (is-true (rss-well-formed-p body)
               "served /rss body must be well-formed XML:~%~A" body)
      (is-true (search "first &amp; only" body)
               "ampersand in title must be entity-escaped over the wire")
      (is-true (search "<![CDATA[" body)
               "post body must be wrapped in CDATA"))))

(test int-blog-page-lists-posts
  "/blog returns text/html listing the registered posts."
  (with-integration-server
    (site.blog-registry:register-post
     (make-instance 'site.blog-post:blog-post
                    :id 3700000000
                    :subject "Integration-listed"
                    :tags '("en")
                    :post (lambda () "<p>body</p>")))
    (multiple-value-bind (body status ct) (int-get "/blog")
      (is (= 200 status))
      (is-true (ct-content-type-is "text/html" ct)
               "HTML pages must advertise text/html (got ~S)" ct)
      (is-true (search "Integration-listed" body)))))

(test int-blog-post-page-renders
  "/blog?id=N returns text/html single-post rendering."
  (with-integration-server
    (site.blog-registry:register-post
     (make-instance 'site.blog-post:blog-post
                    :id 3700000000
                    :subject "One-post"
                    :tags '("en")
                    :post (lambda () "<p>body bits</p>")))
    (multiple-value-bind (body status ct)
        (int-get "/blog?id=3700000000")
      (is (= 200 status))
      (is-true (ct-content-type-is "text/html" ct)
               "single-post Content-Type must be text/html (got ~S)" ct)
      (is-true (search "One-post" body))
      (is-true (search "<article>" body)))))

(test int-blog-page-content-negotiation-returns-note
  "A request for /blog?id=N with Accept: application/activity+json
returns a Note JSON AND a matching Content-Type."
  (with-integration-server
    (site.blog-registry:register-post
     (make-instance 'site.blog-post:blog-post
                    :id 3700000000
                    :subject "fedi-note"
                    :tags '("en" "fedi")
                    :post (lambda () "<p>body</p>")))
    (multiple-value-bind (body status ct)
        (int-get "/blog?id=3700000000" :accept "application/activity+json")
      (is (= 200 status))
      ;; Handler echoes the client's Accept value as Content-Type, which
      ;; is unusual but acceptable so long as it is not text/html.
      (is-false (ct-content-type-is "text/html" ct)
                "content-negotiated response must not be HTML (got ~S)" ct)
      (is-true (search "application/activity+json" ct)
               "Content-Type should advertise activity+json (got ~S)" ct)
      (let ((decoded (cl-json:decode-json-from-string body)))
        (is (equal "Note" (cdr (assoc :type decoded))))))))

(test int-about-page
  "/about returns text/html."
  (with-integration-server
    (multiple-value-bind (body status ct) (int-get "/about")
      (is (= 200 status))
      (is-true (ct-content-type-is "text/html" ct)
               "/about Content-Type must be text/html (got ~S)" ct)
      (is-true (search "About" body)))))

(test int-robots-txt
  "/robots.txt returns text/plain (the canonical robots.txt type)."
  (with-integration-server
    (multiple-value-bind (body status ct) (int-get "/robots.txt")
      (is (= 200 status))
      (is-true (ct-content-type-is "text/plain" ct)
               "robots.txt Content-Type must be text/plain (got ~S)" ct)
      (is-true (search "User-agent: *" body))
      (is-true (search "/blog" body)))))

(test int-health-check
  "/health returns 200 with liveness details. Skip gracefully when
/proc/loadavg is unreadable (non-linux CI)."
  (with-integration-server
    (if (probe-file #P"/proc/loadavg")
        (multiple-value-bind (body status ct) (int-get "/health")
          (declare (ignore ct))
          (is (= 200 status))
          (is-true (search "Alive" body)))
        (pass "no /proc/loadavg on this host, health check skipped"))))

(test int-blog-inbox-rejects-post-without-signature
  "POST /ap/actor/blog/inbox without a Signature header must not 200.
The handler fails signature verification and returns 401."
  (with-integration-server
    (multiple-value-bind (body status ct)
        (int-post "/ap/actor/blog/inbox"
                  "{\"type\":\"Follow\"}"
                  :content-type "application/ld+json")
      (declare (ignore body ct))
      (is (/= 200 status)))))

(test int-blog-inbox-get-returns-empty-body
  "GET /ap/actor/blog/inbox is a valid no-op that returns 200 with an
empty body — some federation peers probe inboxes with GET."
  (with-integration-server
    (multiple-value-bind (body status ct)
        (int-get "/ap/actor/blog/inbox")
      (declare (ignore ct))
      (is (= 200 status))
      (is (equal "" body)))))
