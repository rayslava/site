;;; ActivityPub JSON object builders. Pure functions — no I/O, no
;;; storage, no HTTP. They take a blog-post (or a list of attachments)
;;; and return an alist ready for cl-json encoding. The fedi-*-create
;;; wrappers wrap the encoder for convenience.

(in-package :site.activitypub)

(defun prepare-image-attachments (atts)
  "Build the `attachment' list for an ActivityStreams object from a list
of blog-post-attachment instances."
  `("attachment" . ,(mapcar (lambda (att)
                              (let ((u (url att)))
                                `(("id" . ,u)
                                  ("type" . "Document")
                                  ("mediaType" . "image/jpeg")
                                  ("url" . ,u))))
                            atts)))

(defun prepare-fedi-object (post type)
  "Produce an ActivityStreams object of TYPE (Create, Update) from POST."
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
				 ("to" . "https://www.w3.org/ns/activitystreams#Public")))
		    ,(let ((image-atts (when (slot-boundp post 'attachment)
					 (remove-if-not (lambda (a) (eq (att-type a) 'image))
							(attachment post)))))
		       (when image-atts (prepare-image-attachments image-atts))))))
    message))

(defun fedi-post-create (post)
  "Encode a Create activity for POST as a JSON string."
  (let* ((cl-json::+json-lisp-escaped-chars+
	   (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
    (cl-json:encode-json-alist-to-string (prepare-fedi-object post "Create"))))

(defun prepare-fedi-note (post)
  "Produce a Note object (not wrapped in a Create) for GET responses."
  (let* ((post-id (format nil "https://rayslava.com/blog?id=~A" (id post)))
	 (date  (local-time:format-timestring nil (local-time:universal-to-timestamp (id post))
					      :format '(:year "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2) "Z")))
	 (lang (cond ((member "ru" (tags post) :test #'string=) "ru")
		     ((member "en" (tags post) :test #'string=) "en")
		     (t nil)))
	 (langtag (if lang `(("@language" . ,lang))
		      nil))
	 (content (cl-ppcre:regex-replace-all "\\s*\\\n\\s*" (funcall (post post)) " "))
	 (contentMap (if langtag
			 `("contentMap" . ((,lang . ,content)))
			 nil))
	 (message `(("@context" . ,(if langtag
				       `("https://www.w3.org/ns/activitystreams" ,langtag)
				       "https://www.w3.org/ns/activitystreams"))
		    ("id" . ,post-id)
		    ("type" . "Note")
		    ("published" . ,date)
		    ("actor" . "https://rayslava.com/ap/actor/blog")
		    ("attributedTo" . "https://rayslava.com/ap/actor/blog")
		    ("content" . ,content)
		    ,contentMap
		    ("to" . "https://www.w3.org/ns/activitystreams#Public")
		    ,(let ((image-atts (when (slot-boundp post 'attachment)
					 (remove-if-not (lambda (a) (eq (att-type a) 'image))
							(attachment post)))))
		       (when image-atts (prepare-image-attachments image-atts))))))
    message))

(defun fedi-note-create (post)
  "Encode a standalone Note for POST as a JSON string."
  (let* ((cl-json::+json-lisp-escaped-chars+
	   (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
    (cl-json:encode-json-alist-to-string (prepare-fedi-note post))))

(defun fedi-post-update (post)
  "Encode an Update activity for POST as a JSON string."
  (let ((cl-json::+json-lisp-escaped-chars+
	  (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
    (cl-json:encode-json-alist-to-string (prepare-fedi-object post "Update"))))
