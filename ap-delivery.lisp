;;; ActivityPub delivery: signing and POSTing activities to remote
;;; inboxes, plus the subscriber-facing helpers that iterate over the
;;; subscriber list. The `dex:post` call is routed through *http-send-fn*
;;; so tests can capture outbound traffic without mocking dex itself.

(in-package :site.activitypub)

(defgeneric unsubscribe (subscriber)
  (:documentation "Unsubscribe SUBSCRIBER from blog delivery."))

(defmethod unsubscribe ((subscriber activitypub-subscriber))
  "Delete SUBSCRIBER from storage and stop pushing messages."
  (site.storage:subscriber-delete (actor subscriber)))

(defmethod update-lastpost ((subscriber activitypub-subscriber) newlastpost)
  "Update lastpost number in storage for SUBSCRIBER."
  (site.storage:subscriber-update-lastpost (actor subscriber) newlastpost))

(defun generate-signed-header (keyid inbox domain date hash)
  "Build the `Signature` HTTP header value for a POST to INBOX."
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

(defvar *http-send-fn*
  (lambda (url &key headers content)
    (dex:post url :headers headers :content content :verbose nil))
  "Function invoked to POST a signed activity. Test code rebinds this to
capture requests in memory; production leaves the dex:post default.")

(defun send-signed (actor message)
  "Sign and POST MESSAGE to ACTOR's inbox. Returns whatever the sender
returns on success; nil on common HTTP failures. Network errors are
logged but never raised, so delivery-loop callers can proceed."
  (let* ((actor-uri (quri:uri actor))
	 (target-domain (quri:uri-host actor-uri))
	 (target-user (quri:uri-path actor-uri))
	 (target-inbox (concatenate 'string target-user "/inbox"))
	 (reply-url (concatenate 'string actor "/inbox"))
	 (date (hunchentoot:rfc-1123-date))
	 (hash (concatenate 'string "SHA-256="
			    (base64:usb8-array-to-base64-string
                             (ironclad:digest-sequence
                              :sha256
                              (ironclad:ascii-string-to-byte-array message)))))
	 (keyid "https://rayslava.com/ap/actor/blog#main-key"))
    (handler-case
        (funcall *http-send-fn*
                 reply-url
                 :headers `(("content-type" . "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
			    ("host" . ,target-domain)
			    ("date" . ,date)
			    ("digest" . ,hash)
			    ("accept" . "application/activity+json, application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
			    ("Signature" . ,(generate-signed-header keyid target-inbox target-domain date hash)))
                 :content message)
      (dex:http-request-not-found (e)
        (hunchentoot:log-message* :info "Actor inbox not found (404): ~A (~A)" actor e)
        nil)
      (dex:http-request-forbidden (e)
        (hunchentoot:log-message* :info "Actor inbox forbidden (403): ~A (~A)" actor e)
        nil)
      (dex:http-request-gone (e)
        (hunchentoot:log-message* :info "Actor inbox gone (410): ~A (~A)" actor e)
        nil)
      (error (e)
        (hunchentoot:log-message* :warning "Failed to send signed message to ~A: ~A" actor e)
        nil))))

(defun maybe-deliver-new-post (post)
  "Find subscribers whose lastpost < (id POST) and push POST to each."
  (let ((unnotified (site.storage:subscriber-all-with-lastpost< (id post))))
    (dolist (row unnotified)
      (let ((actor (getf row :actor)))
        (send-signed actor (fedi-post-create post))
        (site.storage:subscriber-update-lastpost actor (id post))
        (hunchentoot:log-message*
         :info "Delivered post ~A to ~A~%" (id post) actor)))
    (length unnotified)))

(defun maybe-deliver-new-posts (posts)
  "For each fedi-tagged post in POSTS (ascending id), deliver it to any
subscribers who haven't seen it yet."
  (let ((fediposts (sort
		    (remove-if-not #'(lambda (post)
				       (member "fedi" (tags post) :test #'equal))
				   posts)
		    #'< :key #'(lambda (p) (id p)))))
    (dolist (post fediposts)
      (maybe-deliver-new-post post))))

(defun maybe-update-post (post)
  "Find subscribers who already received POST and push an Update activity."
  (let ((notified (site.storage:subscriber-all-with-lastpost>= (id post))))
    (dolist (row notified)
      (send-signed (getf row :actor) (fedi-post-update post)))
    (length notified)))
