(defpackage :site.activitypub
  (:use :cl :hunchentoot :cl-who :cl-json
	:asdf :site :site.db-manage :site.config
	:ironclad :trivia :local-time :dexador
	:uuid))

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
(define-easy-handler (actor-blog :uri "/ap/actor/blog"
				 :default-request-type :get)
    ()
  (setf (hunchentoot:content-type*) "application/activity+json")
  (let ((cl-json::+json-lisp-escaped-chars+
	  (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
    (json:encode-json-to-string
     `(("@context" . ("https://www.w3.org/ns/activitystreams" "https://w3id.org/security/v1"))
       ("id" . "https://rayslava.com/ap/actor/blog")
       ("type" . "Person")
       ("preferredUsername" . "blog")
       ("summary" . "Personal blog from rayslava.com")
       ("inbox" . "https://rayslava.com/ap/actor/blog/inbox")
       ("publicKey" . (("id" . "https://rayslava.com/ap/actor/blog#main-key")
		       ("owner" . "https://rayslava.com/ap/actor/blog")
		       ("publicKeyPem" . ,site.config:*activitypub-public-key*)))))))


					; Accept followers
(define-easy-handler (blog-inbox :uri "/ap/actor/blog/inbox"
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

(defun generate-accept (request)
  (let ((reply `(("@context" . "https://www.w3.org/ns/activitystreams")
		 ("id" . ,(string-downcase (format nil "https://rayslava.com/~A" (uuid:make-v4-uuid))))
		 ("type" . "Accept")
		 ("actor" . "https://rayslava.com/ap/actor/blog")
		 ("object" . ,request)))
	(cl-json::+json-lisp-escaped-chars+
	  (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
    (cl-json:encode-json-alist reply)))

(defun generate-signed-header (keyid inbox domain date hash)

  (format nil "keyId=\"~A\",headers=\"(request-target) host date digest\",signature=\"~A\""
	  keyid
	  (base64:usb8-array-to-base64-string
	   (ironclad:sign-message
	    *privkey*
	    (ironclad:digest-sequence :sha256
				      (ironclad:ascii-string-to-byte-array
				       (format nil "(request-target): post ~A~%host: ~A~%date: ~A~%digest: SHA-256=~A"
					       inbox
					       domain
					       date
					       hash)))))))

(defun send-reply (replyurl message)
  (let ((inbox "/inbox")
	(domain "host")
	(date (local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+))
	(hash "sdfg")
	(keyid "https://rayslava.com/ap/actor/blog#main-key"))
    (dex:post replyurl :headers `(("Content-Type" . "application/ld+json")
				  ("(request-target)" . ,inbox)
				  ("host" . ,domain)
				  ("date" . ,date)
				  ("digest" . ,hash)
				  ("Signature" . ,(generate-signed-header keyid inbox domain date hash)))
		       :content message)))
