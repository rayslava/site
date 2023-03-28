(defpackage :site.activitypub
  (:use :cl :hunchentoot :cl-who :cl-json
	:asdf :site :dyna.table-operation :dyna
	:site.db-manage :site.config :site.crypto :site.blog)
  (:export :maybe-deliver-new-posts))

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
       ("url" . "https://rayslava.com/")
       ("type" . "Person")
       ("preferredUsername" . "blog")
       ("name" . "rayslava")
       ("summary" . "Personal blog from rayslava.com")
       ("inbox" . "https://rayslava.com/ap/actor/blog/inbox")
       ("icon" .  (("type" . "Image")
		   ("mediaType" . "image/png")
		   ("url" . "https://rayslava.com/i/apub-avatar.png")))
       ("publicKey" . (("id" . "https://rayslava.com/ap/actor/blog#main-key")
		       ("owner" . "https://rayslava.com/ap/actor/blog")
		       ("publicKeyPem" . ,site.config:*activitypub-public-key-pem*)))))))

(defmacro generate-delete-reply-func (message)
  `(lambda (args)
     (let* ((data-string (hunchentoot:raw-post-data :force-text t))
	    (request-obj (cl-json:decode-json-from-string data-string)))
       (if (string= "Delete" (cdr (assoc :type request-obj)))
	   (progn
	     (hunchentoot:log-message* :info "Delete request came to remove ~A" keyid)
	     (cl-json:encode-json-to-string '(("status" . "ok"))))
	   (progn
	     (setf (return-code*) hunchentoot:+http-authorization-required+)
	     (hunchentoot:log-message* :info "~A~A" ,message keyid)
	     (format nil "~A~A~%" ,message args))))
     (return-from blog-inbox)))

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
	     (flet ((get-subheader-string (name subheaders)
		      (string-trim "\"" (cdr (assoc name subheaders :test #'string-equal)))))
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
		      (keyid (get-subheader-string "keyid" signature-parts))
		      (headers (get-subheader-string "headers" signature-parts))
		      (signature (base64:base64-string-to-usb8-array
				  (get-subheader-string "signature" signature-parts)))
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
				    (handler-bind ((dex:http-request-gone (generate-delete-reply-func "Public key gone for id "))
						   (dex:http-request-not-found (generate-delete-reply-func "Public key not found for id ")))
				      (dex:get keyid :headers
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
		   (hunchentoot:log-message* :info "Signature verification failed for ~A" keyid)
		   (setf (return-code*) hunchentoot:+http-authorization-required+)
		   (format nil "Signature verification failed~%")
		   (return-from blog-inbox))))
	     ;;; Now the actual request can be processed
             (let* ((data-string (hunchentoot:raw-post-data :force-text t))
                    (request-obj (cl-json:decode-json-from-string data-string)))
	       (cond ((string= "Follow" (cdr (assoc :type request-obj)))
		      (send-signed (cdr (assoc :actor request-obj)) (generate-accept request-obj))
		      (let* ((actor (cdr (assoc :actor request-obj)))
			     (attr `(("subscribed" . ,(hunchentoot:rfc-1123-date))))
			     (subscriber (make-instance 'activitypub-subscriber :actor actor
										:attr (cl-json:encode-json-to-string attr))))
			(when (not (nth-value 0 (select-dyna 'activitypub-subscriber
							     (sxql:where (:= :actor actor)))))
			  (save-dyna subscriber)
			  (hunchentoot:log-message* :info "Accepted new follower: ~A. Sending the posts." actor)
			  (maybe-deliver-new-posts))))
		     ((string= "Undo" (cdr (assoc :type request-obj)))
		      (let* ((object (cdr (assoc :object request-obj))))
			(cond ((string= "Follow" (cdr (assoc :type object)))
			       (unsubscribe (make-instance 'activitypub-subscriber :actor (cdr (assoc :actor object))))
			       (hunchentoot:log-message* :info "Unsubscribed user: ~A" (cdr (assoc :actor object)))
			       (cl-json:encode-json-to-string '(("status" . "ok"))))
			      (t (hunchentoot:log-message* :info "Unexpected undo request for object of type ~A received from ~A~%Dump: ~A~%"
							   (cdr (assoc :type object))
							   (cdr (assoc :actor request-obj))
							   request-obj)))))
		     (t (hunchentoot:log-message* :info "Unexpected verified request of type ~A received from ~A~%Dump: ~A~%"
						  (cdr (assoc :type request-obj))
						  (cdr (assoc :actor request-obj))
						  request-obj)))))))))

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
	 (date (hunchentoot:rfc-1123-date))
	 (hash (concatenate 'string "SHA-256="
			    (base64:usb8-array-to-base64-string (ironclad:digest-sequence
								 :sha256
								 (ironclad:ascii-string-to-byte-array message)))))
	 (keyid "https://rayslava.com/ap/actor/blog#main-key"))
    (handler-bind ((dex:http-request-not-found #'dex:ignore-and-continue)
		   (dex:http-request-not-implemented #'dex:ignore-and-continue))
      (dex:post reply-url :headers `(("content-type" . "application/ld+json")
				     ("host" . ,target-domain)
				     ("date" . ,date)
				     ("digest" . ,hash)
				     ("accept" . "application/ld+json; profile=\"http://www.w3.org/ns/activitystreams\"")
				     ("Signature" . ,(generate-signed-header keyid target-inbox target-domain date hash)))
			  :content message
			  :verbose nil))))

;;; Static storage procedure
(defclass activitypub-subscriber ()
  ((actor :key-type :hash
	  :attr-name "actor"
	  :attr-type :S
	  :initarg :actor
	  :accessor actor
	  :documentation "The actor url of subscriber.")
   (lastpost :key-type :range
	     :attr-name "lastpost"
	     :attr-type :N
	     :initarg :lastpost
	     :accessor lastpost
	     :initform 0
	     :documentation "Id of last post sent to this subscriber")
   (attr :key-type :range
	 :attr-name "attr"
	 :attr-type :S
	 :initarg :attr
	 :accessor attr
	 :initform nil
	 :documentation "Sorted plist with additional arguments"))
  (:dyna *dyna*)
  (:table-name "activitypub-subscribers")
  (:metaclass dyna.table-operation::<dyna-table-class>))

(defmethod print-object ((subscriber activitypub-subscriber) out)
  (with-slots (actor lastpost attr) subscriber
    (print-unreadable-object (subscriber out :type t)
      (format out "File '~A' with last post provided ~A ~A" actor lastpost attr))))

;;; Create DynamoDB table if one doesn't exist
(when (not (table-exist-p 'activitypub-subscriber))
  (create-dyna-table 'activitypub-subscriber))

(defgeneric unsubscribe (subscriber)
  (:documentation "Unsubscribe the subscriber from blog."))

(defmethod unsubscribe ((subscriber activitypub-subscriber))
  "Delete the `subscriber' from DynamoDB and stop pushing messages"
  (let ((item (car (select-dyna 'activitypub-subscriber
				(sxql:where (:= :actor (actor subscriber)))))))
    (when item
      (let ((oldposts (lastpost item)))
	(delete-item *dyna* :table-name "activitypub-subscribers"
			    :key `(("actor" . ,(actor item))
				   ("lastpost" . ,oldposts)))))))

(defmethod update-lastpost ((subscriber activitypub-subscriber) newlastpost)
  "Update lastpost number in DynamoDB to `newlastpost' for `subscriber'"
  (let* ((item (car (select-dyna 'activitypub-subscriber
				 (sxql:where (:= :actor (actor subscriber))))))
	 (oldposts (lastpost item)))
    (setf (lastpost item) newlastpost)
    (delete-item *dyna* :table-name "activitypub-subscribers"
			:key `(("actor" . ,(actor item))
			       ("lastpost" . ,oldposts)))
    (save-dyna item)))

(defun prepare-fedi-object (post type)
  "Produces new json object from the `post' of type `type'"
  (let* ((post-id (format nil "https://rayslava.com/blog?id=~A" (id post)))
	 (date  (local-time:format-timestring nil (local-time:universal-to-timestamp (id post))
					      :format '(:year "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2) "Z")))
	 (langtag (cond ((member "ru" (tags post) :test #'string=) '(("@language" . "ru")))
			((member "en" (tags post) :test #'string=) '(("@language" . "en")))
			(t nil)))
	 (message `(("@context" . ,(if langtag
				       `("https://www.w3.org/ns/activitystreams" ,langtag)
				       "https://www.w3.org/ns/activitystreams"))
		    ("id" . ,post-id)
		    ("type" . ,type)
		    ("actor" . "https://rayslava.com/ap/actor/blog")
		    ("object" . (("id" . ,post-id)
				 ("type" . "Note")
				 ("published" . ,date)
				 ("attributedTo" . "https://rayslava.com/ap/actor/blog")
				 ("content" . ,(cl-ppcre:regex-replace-all "\\s*\\\n\\s*" (funcall (post post)) " "))
				 ("to" . "https://www.w3.org/ns/activitystreams#Public"))))))
    message))

(defun fedi-post-create (post)
  "Produces new json 'Create' activity from the `post'

TODO: Check and support mastodon formatting, currently it seems that newlines
are processed as plain text, not as in HTML"
  (let* ((cl-json::+json-lisp-escaped-chars+
	   (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
    (cl-json:encode-json-alist-to-string (prepare-fedi-object post "Create"))))

(defun fedi-post-update (post)
  "Produces new json 'Update' activity from the `post'"
  (let ((cl-json::+json-lisp-escaped-chars+
	  (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
    (cl-json:encode-json-alist-to-string (prepare-fedi-object post "Update"))))

(defun maybe-deliver-new-post (post)
  "Find all the subscribers who didn't recieve the `post' yet and push the post
to corresponding actor"
  (let ((unnotified (select-dyna 'activitypub-subscriber
				 (sxql:where (:< :lastpost (id post))))))
    (mapcar #'(lambda (subscriber)
		(hunchentoot:log-message* :info "Delivered post ~A to ~A: ~A"
					  (id post)
					  (actor subscriber)
					  (send-signed (actor subscriber) (fedi-post-create post)))
		(update-lastpost subscriber (id post)))
	    unnotified)
    (length unnotified)))

(defun maybe-deliver-new-posts ()
  "Check if there are new posts to deliver to subscribers"
  (let ((fediposts (sort
		    (remove-if-not #'(lambda (post)
				       (member "fedi" (tags post) :test #'equal))
				   site.blog::*blog-posts*)
		    #'< :key #'(lambda (p) (id p)))))
    (dolist (post fediposts)
      (maybe-deliver-new-post post))))

(defun maybe-update-post (post)
  "Find all the subscribers who recieved the `post' already and push the new
version to corresponding actor"
  (let ((notified (select-dyna 'activitypub-subscriber
			       (sxql:where (:>= :lastpost (id post))))))
    (mapcar #'(lambda (subscriber)
		(hunchentoot:log-message* :info "Updating post ~A to ~A: ~A"
			(id post)
			(actor subscriber)
			(send-signed (actor subscriber) (fedi-post-update post))))
	    notified)
    (length notified)))
