;;; ActivityPub inbox handler.
;;;
;;; The handler's job is: read POST body, verify HTTP Signature, then
;;; dispatch on the activity type. Signature verification needs a live
;;; request so the dex:get for the signer's public key can short-circuit
;;; on HTTP 410/404 via handler-bind. But the type-dispatch below is
;;; pure: the input is a decoded alist + the raw body string, and every
;;; side-effect goes through site.storage. So handle-inbox-activity is
;;; directly unit-testable against the memory backend.

(in-package :site.activitypub)

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

(defun handle-inbox-activity (request-obj data-string)
  "Dispatch on REQUEST-OBJ's :type. DATA-STRING is the raw JSON body (for
persistence). Returns the JSON reply string. All state changes go
through site.storage, so this is unit-testable with a memory backend."
  (cond
    ;; Follow: save subscriber, accept, deliver backlog.
    ((string= "Follow" (cdr (assoc :type request-obj)))
     (send-signed (cdr (assoc :actor request-obj)) (generate-accept request-obj))
     (let ((actor (cdr (assoc :actor request-obj)))
	   (attr `(("subscribed" . ,(hunchentoot:rfc-1123-date)))))
       (unless (site.storage:subscriber-find actor)
	 (site.storage:subscriber-save actor attr)
	 (hunchentoot:log-message* :info "Accepted new follower: ~A. Sending the posts." actor)
	 (maybe-deliver-new-posts (all-posts))))
     (cl-json:encode-json-to-string '(("status" . "ok"))))
    ;; Undo: could be Undo Follow (unsubscribe), Undo Like/Announce (dislike).
    ((string= "Undo" (cdr (assoc :type request-obj)))
     (let* ((object (cdr (assoc :object request-obj))))
       (cond ((string= "Follow" (cdr (assoc :type object)))
	      (unsubscribe (make-instance 'activitypub-subscriber
                                          :actor (cdr (assoc :actor object))))
	      (hunchentoot:log-message* :info "Unsubscribed user: ~A" (cdr (assoc :actor object)))
	      (cl-json:encode-json-to-string '(("status" . "ok"))))
	     ((or (string= "Like" (cdr (assoc :type object)))
		  (string= "Announce" (cdr (assoc :type object))))
	      (dislike (cdr (assoc :id object)))
	      (hunchentoot:log-message* :info "Removed ~A for: ~A"
					(cdr (assoc :type object))
					(cdr (assoc :object object)))
	      (cl-json:encode-json-to-string '(("status" . "ok"))))
	     (t (hunchentoot:log-message* :info "Unexpected undo request for object of type ~A received from ~A~%Dump: ~A~%"
					  (cdr (assoc :type object))
					  (cdr (assoc :actor request-obj))
					  request-obj)
		(cl-json:encode-json-to-string '(("status" . "ok")))))))
    ;; Like / Announce: record the reaction.
    ((or (string= "Like" (cdr (assoc :type request-obj)))
	 (string= "Announce" (cdr (assoc :type request-obj))))
     (let ((event-type (cdr (assoc :type request-obj)))
	   (id (cdr (assoc :id request-obj)))
	   (actor (cdr (assoc :actor request-obj)))
	   (object-id (cdr (assoc :object request-obj))))
       (site.storage:event-save
	(list :id id :object-id object-id :published 0
	      :event-type event-type :event data-string))
       (hunchentoot:log-message* :info "Received like for ~A from ~A" object-id actor))
     (cl-json:encode-json-to-string '(("status" . "ok"))))
    ;; Delete: log, do not persist.
    ((string= "Delete" (cdr (assoc :type request-obj)))
     (let ((object-id (cdr (assoc :object request-obj))))
       (hunchentoot:log-message* :info "Received delete request for ~A" object-id))
     (cl-json:encode-json-to-string '(("status" . "ok"))))
    ;; Fallthrough: treat as an unexpected activity worth saving for audit.
    (t
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
	    (reply-str (cdr (assoc :in-reply-to apub-object))))
       (site.storage:event-save
	(list :id id :object-id object-id :published published
	      :event-type event-type :event data-string
	      :reply-to reply-str)))
     (cl-json:encode-json-to-string '(("status" . "ok"))))))

(define-easy-handler (blog-inbox :uri "/ap/actor/blog/inbox"
				 :default-request-type :post)
    ()
  (setf (hunchentoot:content-type*) "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
  (let ((cl-json::+json-lisp-escaped-chars+
	  (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
    (let ((request-type (hunchentoot:request-method hunchentoot:*request*)))
      (cond
        ((eq request-type :get) "")
        ((eq request-type :post)
         ;; Signature verification — the dex:get for the signer's public key
         ;; stays here so handler-bind can short-circuit 410/404 into a Delete
         ;; reply. Parsing and message-building are pure and live in
         ;; site.ap-signature.
         (let* ((http-headers (headers-in hunchentoot:*request*))
                (sig (parse-signature-header (cdr (assoc :signature http-headers))))
                (keyid (signature-keyid sig))
                (covered (cl-ppcre:split " " (signature-headers sig)))
                (message-to-check
                  (build-signed-message "post" "/ap/actor/blog/inbox"
                                        http-headers covered))
                (userprofile
                  (cl-json:decode-json-from-string
                   (handler-bind ((dex:http-request-gone
                                    (generate-delete-reply-func "Public key gone for id "))
                                  (dex:http-request-not-found
                                    (generate-delete-reply-func "Public key not found for id ")))
                     (dex:get keyid
                              :headers '(("accept" . "application/activity+json, application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\""))
                              :force-string t
                              :verbose nil))))
                (pem (cdr (assoc :public-key-pem
                                 (cdr (assoc :public-key userprofile)))))
                (public-key (extract-public-key-from-actor-pem pem)))
           (unless (rsassa-pkcs1-v1_5-verify
                    public-key
                    (ironclad:ascii-string-to-byte-array message-to-check)
                    (signature-bytes sig)
                    :sha256)
             (hunchentoot:log-message* :info "Signature verification failed for ~A" keyid)
             (setf (return-code*) hunchentoot:+http-authorization-required+)
             (format nil "Signature verification failed~%")
             (return-from blog-inbox)))
         ;; Signature verified; dispatch on activity type.
         (let* ((data-string (hunchentoot:raw-post-data :force-text t))
                (request-obj (cl-json:decode-json-from-string data-string)))
           (handle-inbox-activity request-obj data-string)))))))
