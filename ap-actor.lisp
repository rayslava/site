;;; ActivityPub actor-facing endpoints: the lookup shims Mastodon
;;; expects (webfinger, accounts/lookup, actor JSON, outbox pagination)
;;; and the `generate-accept` helper for Follow responses.

(in-package :site.activitypub)

					; Mastodon lookup workaround
(define-easy-handler (masto-lookup :uri "/api/v1/accounts/lookup/"
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
	      ("links"
	       (("rel" . "http://webfinger.net/rel/profile-page")
		("type" . "text/html")
		("href" . "https://rayslava.com/blog"))
	       (("rel" . "http://webfinger.net/rel/avatar")
		("type" . "image/png")
		("href" . "https://rayslava.com/i/apub-avatar.png"))
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
       ("outbox" . "https://rayslava.com/ap/actor/blog/outbox")
       ("icon" .  (("type" . "Image")
		   ("mediaType" . "image/png")
		   ("url" . "https://rayslava.com/i/apub-avatar.png")))
       ("attachment" . ((("type" . "PropertyValue")
			 ("name" . "Homepage")
			 ("value" . "https://rayslava.com/blog"))))
       ("publicKey" . (("id" . "https://rayslava.com/ap/actor/blog#main-key")
		       ("owner" . "https://rayslava.com/ap/actor/blog")
		       ("publicKeyPem" . ,site.config:*activitypub-public-key-pem*)))))))

(defun generate-outbox-collection (page)
  "Build the OrderedCollection alist for the outbox page PAGE."
  (let* ((all-posts (site.blog-registry:all-posts))
         (fedi-posts (sort
                      (copy-list
                       (remove-if-not #'(lambda (post)
                                          (member "fedi" (tags post) :test #'equal))
                                      all-posts))
                      #'> :key #'(lambda (p) (id p))))
         (items-per-page 20)
         (total-items (length fedi-posts))
         (start-idx (min (* (1- page) items-per-page) total-items))
         (page-items (subseq fedi-posts
                             start-idx
                             (min (+ start-idx items-per-page) total-items)))
         (next-page-url (if (< (+ start-idx items-per-page) total-items)
                            (format nil "https://rayslava.com/ap/actor/blog/outbox?page=~A" (1+ page))
                            nil))
         (prev-page-url (if (> page 1)
                            (format nil "https://rayslava.com/ap/actor/blog/outbox?page=~A" (1- page))
                            nil)))
    `(("@context" . "https://www.w3.org/ns/activitystreams")
      ("id" . "https://rayslava.com/ap/actor/blog/outbox")
      ("type" . "OrderedCollection")
      ("totalItems" . ,total-items)
      ("first" . "https://rayslava.com/ap/actor/blog/outbox?page=1")
      ("last" . ,(format nil "https://rayslava.com/ap/actor/blog/outbox?page=~A"
                         (ceiling total-items items-per-page)))
      ,@(when (= page 1)
          `(("orderedItems" . ,(mapcar #'(lambda (post)
                                           (cdr (assoc "object" (prepare-fedi-object post "Create") :test #'string=)))
                                       page-items))))
      ,@(when (> page 1)
          `(("type" . "OrderedCollectionPage")
            ("partOf" . "https://rayslava.com/ap/actor/blog/outbox")
            ("orderedItems" . ,(mapcar #'(lambda (post)
                                           (cdr (assoc "object" (prepare-fedi-object post "Create") :test #'string=)))
                                       page-items))))
      ,@(when next-page-url `(("next" . ,next-page-url)))
      ,@(when prev-page-url `(("prev" . ,prev-page-url))))))

(define-easy-handler (blog-outbox :uri "/ap/actor/blog/outbox"
                                  :default-request-type :get)
    ((page :parameter-type 'integer :init-form 1))
  (setf (hunchentoot:content-type*) "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
  (let ((cl-json::+json-lisp-escaped-chars+
          (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
    (json:encode-json-to-string
     (generate-outbox-collection page))))

(define-easy-handler (outbox-post :uri "/ap/actor/blog/outbox/post"
                                  :default-request-type :get)
    ((id :parameter-type 'integer))
  (setf (hunchentoot:content-type*) "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
  (let* ((post (site.blog-registry:find-post-by-id id))
         (cl-json::+json-lisp-escaped-chars+
           (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
    (if (and post (member "fedi" (tags post) :test #'equal))
        (cl-json:encode-json-alist-to-string
         (prepare-fedi-object post "Create"))
        (progn
          (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
          (cl-json:encode-json-alist-to-string '(("error" . "Post not found")))))))

(defun generate-accept (request)
  "Produce an Accept activity wrapping REQUEST, as a JSON string."
  (let ((reply `(("@context" . "https://www.w3.org/ns/activitystreams")
		 ("id" . ,(string-downcase (format nil "https://rayslava.com/~A" (uuid:make-v4-uuid))))
		 ("type" . "Accept")
		 ("actor" . "https://rayslava.com/ap/actor/blog")
		 ("object" . ,request)))
	(cl-json::+json-lisp-escaped-chars+
	  (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))
    (cl-json:encode-json-alist-to-string reply)))
