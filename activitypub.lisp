(defpackage :site.activitypub
  (:use :cl :hunchentoot :cl-who :cl-json
	:asdf :site :site.db-manage :site.config :site.crypto))

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
		       ("publicKeyPem" . ,site.config:*activitypub-public-key-pem*)))))))


(defmacro fail (&body body)
  `(throw :return (progn ,@body)))

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
	     ;;; Now we need to perform a signature verification
	     (let* ((http-headers (headers-in hunchentoot:*request*))
		    (signature-line (cdr (assoc :signature http-headers)))
		    (lines (cl-ppcre:split "," signature-line))
		    (line-parts (reduce 'nconc
					(mapcar (lambda (l)
						  (coerce
						   (nth-value 1
							      (cl-ppcre:scan-to-strings "(\\w*)=(.*\\\")$" l))
						   'list))
						lines)))
		    (signature-parts
		      (loop for (head . tail) on line-parts by #'cddr
			    collect (cons head (car tail))))
		    (keyid (string-trim "\"" (cdr (assoc "keyid" signature-parts :test #'string-equal))))
		    (headers (string-trim "\"" (cdr (assoc "headers" signature-parts :test #'string-equal))))
		    (signature (base64:base64-string-to-usb8-array
				(string-trim "\"" (cdr (assoc "signature" signature-parts :test #'string-equal)))))
		    (checked-headers (with-output-to-string (s)
				       (mapcar #'(lambda (hdr)
						   (format s "~%~A: ~A"
							   hdr
							   (cdr
							    (assoc (intern (string-upcase hdr) :keyword)
								   http-headers))))
					       (cdr (cl-ppcre:split " " headers)))
				       s))
		    (message-to-check (concatenate 'string
						   "(request-target): post /ap/actor/blog/inbox"
						   checked-headers))
		    (userprofile (cl-json:decode-json-from-string
				  (handler-bind ((dex:http-request-gone #'(lambda (args)
									    (fail
									      (setf (return-code*) hunchentoot:+http-authorization-required+)
									      (format nil "Can't load signature~A~%" args)))))
				    (dex:get keyid :verbose t
						   :headers
						   '(("accept" . "application/ld+json; profile=\"http://www.w3.org/ns/activitystreams\""))))))
		    (key (assoc :public-key userprofile))
		    (pem (cdr (assoc :public-key-pem (cdr key))))
		    (public-key (trivia:match
				    (asn1:decode
				     (base64:base64-string-to-usb8-array
				      (cl-ppcre:regex-replace-all "\\n"
								  (cl-ppcre:regex-replace-all ".*----\\n" pem "") "")))
				  ((asn1:rsa-public-key-info n e)
				   (ironclad:make-public-key :rsa :n n :e e)))))
	       (unless
		   (rsassa-pkcs1-v1_5-verify public-key
					     (ironclad:ascii-string-to-byte-array message-to-check)
					     signature
					     :sha256)
		 (fail
		   (print (format nil "Signature verification failed~%"))
		   (setf (return-code*) hunchentoot:+http-authorization-required+)
		   (format nil "Signature verification failed~%"))))

             (let* ((data-string (hunchentoot:raw-post-data :force-text t))
                    (request-obj (cl-json:decode-json-from-string data-string)))
	       (print (format nil "~A of type ~A~%Checking signature~%" request-obj (cdr (assoc :type request-obj))))
	       (when (string= "Follow" (cdr (assoc :type request-obj)))
		 (send-signed (cdr (assoc :actor request-obj)) (generate-accept request-obj)))
	       ""))))))

(defun generate-accept (request)
  (let ((reply `(("@context" . "https://www.w3.org/ns/activitystreams")
		 ("id" . ,(string-downcase (format nil "https://rayslava.com/~A" (uuid:make-v4-uuid))))
		 ("type" . "Accept")
		 ("actor" . "https://rayslava.com/ap/actor/blog")
		 ("object" . ,request)))
	(cl-json::+json-lisp-escaped-chars+
	  (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
    (cl-json:encode-json-alist-to-string reply)))

(defun generate-signed-header (keyid inbox domain date hash)
  (format nil "keyId=\"~A\",algorithm=\"rsa-sha256\",headers=\"(request-target) host date digest\",signature=\"~A\""
	  keyid
	  (base64:usb8-array-to-base64-string
	   (rsassa-pkcs1-v1_5-sign
	    site.config:*activitypub-private-key*
	    (ironclad:ascii-string-to-byte-array
	     (format nil "(request-target): post ~A~%host: ~A~%date: ~A~%digest: ~A"
		     inbox
		     domain
		     date
		     hash))
	    :sha256))))

(defun send-signed (actor message)
  (let* ((actor-uri (quri:uri actor))
	 (target-domain (quri:uri-domain actor-uri))
	 (target-user (quri:uri-path actor-uri))
	 (target-inbox (concatenate 'string target-user "/inbox"))
	 (reply-url (concatenate 'string actor "/inbox"))
					;	 (date (local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+))
	 (date (hunchentoot:rfc-1123-date))
	 (hash (concatenate 'string "SHA-256="
			    (base64:usb8-array-to-base64-string (ironclad:digest-sequence
								 :sha256
								 (ironclad:ascii-string-to-byte-array message)))))
	 (keyid "https://rayslava.com/ap/actor/blog#main-key"))
    (handler-bind ((dex:http-request-not-found #'dex:ignore-and-continue)
		   (dex:http-request-not-implemented #'dex:ignore-and-continue))
      (dex:post reply-url :headers `(("content-type" . "application/ld+json")
					;				      ("@request-target" . ,(concatenate 'string "post " target-inbox))
				     ("host" . ,target-domain)
				     ("date" . ,date)
				     ("digest" . ,hash)
				     ("accept" . "application/ld+json; profile=\"http://www.w3.org/ns/activitystreams\"")
				     ("Signature" . ,(generate-signed-header keyid target-inbox target-domain date hash)))

		:content message
		:verbose t))))


;;;; Posting message like that
;; (let ((message '(("@context" . "https://www.w3.org/ns/activitystreams")
;; 			  ("id" . "https://rayslava.com/blog/create-test-message")
;; 			  ("type" . "Create")
;; 			  ("actor" . "https://rayslava.com/ap/actor/blog")
;; 			  ("object" . (("id" . "https://rayslava.com/blog/test-message")
;; 				       ("type" . "Note")
;; 				       ("published" . "2023-03-14T17:30:55Z")
;; 				       ("attributedTo" . "https://rayslava.com/ap/actor/blog")
;; 				       ("content" . "<p>Testing ActivityPub from own LISP server</p>")
;; 				       ("to" . "https://www.w3.org/ns/activitystreams#Public")))))
;; 	       (cl-json::+json-lisp-escaped-chars+
;; 		 (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
;; 	   (send-signed "https://lor.sh/users/rayslava" (cl-json:encode-json-alist-to-string message)))
