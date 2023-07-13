(defpackage :site.activitypub
  (:use :cl :hunchentoot :cl-who :cl-json
	:asdf :site :dyna.table-operation :dyna
	:site.db-manage :site.config :site.crypto :site.blog-post)
  (:shadowing-import-from :cl-json-helper :json-bool)
  (:export :maybe-deliver-new-posts :reactions-number :direct-replies :get-all-replies :flatten-replies :fedi-note-create :fedi-post-create))

(in-package :site.activitypub)

					; Mastodon lookup workaround
(define-easy-handler (webfinger :uri "/api/v1/accounts/lookup"
				:default-request-type :get)
    ((acct :parameter-type 'string))
  (setf (hunchentoot:content-type*) "application/jrd+json")
  (cond ((or (equalp acct "blog")
	     (equalp acct "blog@rayslava.com"))
	 (let ((cl-json::+json-lisp-escaped-chars+
		 (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
	   (json:encode-json-to-string
	    `(("id" . "1")
	      ("username" . "rayslava")
	      ("acct" . ,acct)
	      ("display_name" . "rayslava")
	      ("bot" . ,(json-bool nil))
	      ("discoverable" . ,(json-bool nil))
	      ("group" . ,(json-bool nil))
	      ("locked" . ,nil)
	      ("avatar" . "https://rayslava.com/i/apub-avatar.png")
	      ("avatar_static" . "https://rayslava.com/i/apub-avatar.png")
	      ("url" . "https://rayslava.com/blog")
	      ("header" . "")
	      ("header_static" . "")
	      ("noindex" . ,(json-bool t))))))))

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
     `(("@context" . ("https://www.w3.org/ns/activitystreams" "https://w3id.org/security/v1"
							      (("schema". "http://schema.org#")
							       ("PropertyValue" . "schema:PropertyValue")
							       ("value" . "schema:value"))))
       ("id" . "https://rayslava.com/ap/actor/blog")
       ("url" . "https://rayslava.com/blog")
       ("type" . "Person")
       ("preferredUsername" . "blog")
       ("name" . "rayslava")
       ("summary" . "Personal blog from rayslava.com")
       ("inbox" . "https://rayslava.com/ap/actor/blog/inbox")
       ("icon" .  (("type" . "Image")
		   ("mediaType" . "image/png")
		   ("url" . "https://rayslava.com/i/apub-avatar.png")))
       ("attachment" . ((("type" . "PropertyValue")
			 ("name" . "Homepage")
			 ("value" . "https://rayslava.com/blog"))))
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
			  (maybe-deliver-new-posts (find-symbol "*blog-posts*" :site.blog)))))
		     ((string= "Undo" (cdr (assoc :type request-obj)))
		      (let* ((object (cdr (assoc :object request-obj))))
			(cond ((string= "Follow" (cdr (assoc :type object)))
			       (unsubscribe (make-instance 'activitypub-subscriber :actor (cdr (assoc :actor object))))
			       (hunchentoot:log-message* :info "Unsubscribed user: ~A" (cdr (assoc :actor object)))
			       (cl-json:encode-json-to-string '(("status" . "ok"))))
			      ((or
				(string= "Like" (cdr (assoc :type object)))
				(string= "Announce" (cdr (assoc :type object))))
			       (dislike (cdr (assoc :id object)))
			       (hunchentoot:log-message* :info "Removed ~A for: ~A"
							 (cdr (assoc :type object))
							 (cdr (assoc :object object)))
			       (cl-json:encode-json-to-string '(("status" . "ok"))))
			      (t (hunchentoot:log-message* :info "Unexpected undo request for object of type ~A received from ~A~%Dump: ~A~%"
							   (cdr (assoc :type object))
							   (cdr (assoc :actor request-obj))
							   request-obj)))))
		     ((or (string= "Like" (cdr (assoc :type request-obj)))
			  (string= "Announce" (cdr (assoc :type request-obj))))
		      (let* ((event-type (cdr (assoc :type request-obj)))
			     (id (cdr (assoc :id request-obj)))
			     (actor (cdr (assoc :actor request-obj)))
			     (object-id (cdr (assoc :object request-obj)))
			     (event-body data-string)
			     (event (make-instance 'activitypub-event
						   :id id
						   :object-id object-id
						   :published 0
						   :event-type event-type
						   :event event-body)))
			(save-dyna event)
			(hunchentoot:log-message* :info "Received like for ~A from ~A" object-id actor))
		      (cl-json:encode-json-to-string '(("status" . "ok"))))
		     ((string= "Delete" (cdr (assoc :type request-obj)))
		      (let* ((event-type (cdr (assoc :type request-obj)))
			     (id (cdr (assoc :id request-obj)))
			     (actor (cdr (assoc :actor request-obj)))
			     (object-id (cdr (assoc :object request-obj)))
			     (event-body data-string)
			     (event (make-instance 'activitypub-event
						   :id id
						   :object-id object-id
						   :published 0
						   :event-type event-type
						   :event event-body)))
			(save-dyna event)
			(hunchentoot:log-message* :info "Received delete request for ~A from ~A" object-id actor))
		      (cl-json:encode-json-to-string '(("status" . "ok"))))
		     (t (progn
			  (hunchentoot:log-message* :info "Unexpected verified request of type ~A received from ~A, placing into database~%Dump: ~A~%"
						    (cdr (assoc :type request-obj))
						    (cdr (assoc :actor request-obj))
						    request-obj)
			  (let* ((event-type (cdr (assoc :type request-obj)))
				 (id (cdr (assoc :id request-obj)))
				 (apub-object (cdr (assoc :object request-obj)))
				 (object-id (cdr (assoc :id apub-object)))
				 (pubstr (cdr (assoc :published request-obj)))
				 (published (if pubstr
						(local-time:timestamp-to-universal (local-time:parse-timestring pubstr))
						0))
				 (reply-str (cdr (assoc :in-reply-to apub-object)))
				 (event-body data-string)
				 (event (make-instance 'activitypub-event
						       :id id
						       :object-id object-id
						       :published published
						       :event-type event-type
						       :event event-body
						       :reply-to reply-str)))
			    (save-dyna event))
			  (cl-json:encode-json-to-string '(("status" . "ok"))))))))))))

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

(defun prepare-fedi-note (post)
  "Produces new json object from the `post' of type Note to send it for GET"
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
		    ("type" . "Note")
		    ("published" . ,date)
		    ("actor" . "https://rayslava.com/ap/actor/blog")
		    ("attributedTo" . "https://rayslava.com/ap/actor/blog")
		    ("content" . ,(cl-ppcre:regex-replace-all "\\s*\\\n\\s*" (funcall (post post)) " "))
		    ("to" . "https://www.w3.org/ns/activitystreams#Public"))))
    message))

(defun fedi-note-create (post)
  "Produces new json from the `post'"
  (let* ((cl-json::+json-lisp-escaped-chars+
	   (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
    (cl-json:encode-json-alist-to-string (prepare-fedi-note post))))

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
		(update-lastpost subscriber (id post)))
	    unnotified)
    (length unnotified)))

(defun maybe-deliver-new-posts (posts)
  "Check if there are new posts to deliver to subscribers"
  (let ((fediposts (sort
		    (remove-if-not #'(lambda (post)
				       (member "fedi" (tags post) :test #'equal))
				   posts)
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

;;; Static storage procedure
(defclass activitypub-event ()
  ((id :key-type :hash
       :attr-name "id"
       :attr-type :S
       :initarg :id
       :accessor id
       :documentation "ID of the event")
   (published :key-type :range
	      :attr-name "published"
	      :attr-type :N
	      :initarg :published
	      :accessor published
	      :initform 0
	      :documentation "Timestamp of event from published field")
   (event-type :key-type :range
	       :attr-name "eventtype"
	       :attr-type :S
	       :initarg :event-type
	       :accessor event-type
	       :initform nil
	       :documentation "Type of the event")
   (object-id :key-type :hash
	      :attr-name "objectid"
	      :attr-type :S
	      :initarg :object-id
	      :accessor object-id
	      :documentation "ID of the object event is related to")
   (reply-to  :key-type :hash
	      :attr-name "replyto"
	      :attr-type :S
	      :initarg :reply-to
	      :accessor reply-to
	      :initform nil
	      :documentation "ID of the object to reply, if exists")
   (event :key-type :range
	  :attr-name "event"
	  :attr-type :S
	  :initarg :event
	  :accessor event
	  :initform nil
	  :documentation "Full event body"))
  (:dyna *dyna*)
  (:table-name "activitypub-events")
  (:metaclass dyna.table-operation::<dyna-table-class>))

;;; TODO: IMPLEMENT
(defmethod print-object ((event activitypub-event) out)
  (with-slots (id published event-type) event
    (print-unreadable-object (event out :type t)
      (format out "ActivityPub event '~A' of type ~A published at ~A" id event-type published))))

(defun dislike (likeid)
  "Delete the Like or Announce event from DynamoDB"
  (let ((item (car (select-dyna 'activitypub-event (sxql:where (:= :id likeid))))))
    (when item
      (delete-item *dyna* :table-name "activitypub-events"
			  :key `(("id" . ,(id item))
				 ("published" . 0))))))

(defun reactions-number (id reaction-type)
  "Get number of reactions `reaction-type' (Like or Announce) for post with `id'"
  (let ((counts
	  (nth-value 1 (scan *dyna* :table-name "activitypub-events"
				    :filter-expression "objectid = :id AND eventtype = :type"
				    :expression-attribute-values `((":id" . ,(format nil "https://rayslava.com/blog?id=~A" id))
								   (":type" . ,reaction-type))
				    :select "COUNT"))))
    (cdr (assoc "count" (cdr counts) :test #'string-equal))))

;;; Create DynamoDB table if one doesn't exist
(when (not (table-exist-p 'activitypub-event))
  (create-dyna-table 'activitypub-event))

(defun direct-replies (id)
  "Get comments for `id' which came as replies"
  (let* ((response
	  (nth-value 1 (scan *dyna* :table-name "activitypub-events"
				    :filter-expression "replyto = :id AND eventtype = :type"
				    :expression-attribute-values `((":id" . ,(format nil "https://rayslava.com/blog?id=~A" id))
								   (":type" . "Create")))))
	 (replies (cdr (assoc "items" (cdr response) :test #'string-equal)))
	 (result nil))
    (dolist (reply-obj replies result)
      (let*  ((reply (cl-json:decode-json-from-string (cdr (assoc "s"
								 (cddr (assoc "event" (cdr reply-obj) :test #'string-equal)) :test #'string-equal))))
	      (object (cdr (assoc :object reply)))
	      (actor (cdr (assoc :attributed-to object)))
	      (url (cdr (assoc :url object)))
	      (published (local-time:timestamp-to-universal (local-time:parse-timestring (cdr (assoc :published object)))))
	      (content (cdr (assoc :content object)))
	      (to (cdr (assoc :to object)))
	      (public (if (member "https://www.w3.org/ns/activitystreams#Public" to :test #'string-equal)
			  t
			  nil)))
	(push (pairlis '(:id :actor :url :published :content :public)
		       (id list actor url published content public))
	      result)
	result))))

(defun get-all-replies (post-id)
  "Retrieve the full list of replies for the given post ID"
  (let ((replies (direct-replies post-id))
        (all-replies '()))
    (dolist (reply replies)
      (push reply all-replies)
      (when (getf reply :id)
        (setf (getf reply :replies) (get-all-replies (getf reply :id)))
        (dolist (nested-reply (getf reply :replies))
          (push nested-reply all-replies))))
    (nreverse all-replies)))

(defun flatten-replies (replies)
  "Flatten the nested list of replies into a flat list"
  (let ((flat-replies '()))
    (dolist (reply replies)
      (push reply flat-replies)
      (when (getf reply :replies)
        (setf (getf reply :replies) (flatten-replies (getf reply :replies)))
        (dolist (nested-reply (getf reply :replies))
          (push nested-reply flat-replies))))
    (nreverse flat-replies)))

;;; Create DynamoDB table if one doesn't exist
(when (not (table-exist-p 'activitypub-event))
  (create-dyna-table 'activitypub-event))
