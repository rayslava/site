(defpackage :site.activitypub
  (:use :cl :hunchentoot :cl-who :cl-json
	:asdf :site :site.db-manage :site.config))

(in-package :site.activitypub)

					; WebFinger to support ActivityPub
(define-easy-handler (webfinger :uri "/.well-known/webfinger"
				:default-request-type :get)
    ((resource :parameter-type 'string))
  (setf (hunchentoot:content-type*) "application/jrd+json")
  (cond ((equalp resource "acct:blog@rayslava.com")
	 (let ((cl-json::+json-lisp-escaped-chars+
		 (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
	   (json:encode-json-to-string
	    '(("subject" . "acct:blog@rayslava.com")
	      ("aliases" . ("https://rayslava.com/blog" "https://rayslava.com/ap/actor/blog"))
	      ("links" (("rel" . "http://webfinger.net/rel/profile-page")
			("type" . "text/html")
			("href" . "https://rayslava.com/blog"))
	       (("rel" . "self")
		("type" . "application/activity+json")
		("href" . "https://rayslava.com/ap/actor/blog")))))))))

					; Actor to support ActivityPub
(define-easy-handler (actor :uri "/ap/actor/blog"
			    :default-request-type :get)
    ()
  (setf (hunchentoot:content-type*) "application/activity+json")
  (let ((cl-json::+json-lisp-escaped-chars+
	  (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
    (json:encode-json-to-string
     `(("@context" . ("https://www.w3.org/ns/activitystreams" "https://w3id.org/security/v1"))
       ("id" . "https://rayslava.com/ap/actor/blog")
       ("type" . "Person")
       ("preferredUsername" . "rayslava")
       ("summary" . "Personal blog from rayslava.com")
       ("inbox" . "https://rayslava.com/ap/actor/blog/inbox")
       ("publicKey" . (("id" . "https://rayslava.com/ap/actor/blog#main-key")
		       ("owner" . "https://rayslava.com/ap/actor/blog")
		       ("publicKeyPem" . ,site.config:*activitypub-public-key*)))))))


					; Accept followers
(define-easy-handler (actor :uri "/ap/actor/blog/inbox"
			    :default-request-type :post)
    ()
  (setf (hunchentoot:content-type*) "application/ld+json")
  (let ((cl-json::+json-lisp-escaped-chars+
	  (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))

    (let ((request-type (hunchentoot:request-method hunchentoot:*request*)))
      (cond ((eq request-type :get) "" );; handle get request
            ((eq request-type :post)
             (let* ((data-string (hunchentoot:raw-post-data :force-text t))
                    (json-obj (cl-json:decode-json-from-string data-string))) ;; use jsown to parse the string
	       (print (format nil "JSON: ~A\n"json-obj))
	       ""))))))
