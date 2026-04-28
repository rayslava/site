(in-package :site.tests)

(in-suite all-tests)

(defun objects-fixture-post (&key (id 3700000000)
                                  (subject "hello")
                                  (tags '("en"))
                                  (body "<p>hello, world</p>"))
  (make-instance 'site.blog-post:blog-post
                 :id id
                 :subject subject
                 :tags tags
                 :post (let ((s body)) (lambda () s))))

(test ap-objects-prepare-fedi-object-shape
  "prepare-fedi-object builds an alist with the expected @context, id,
type, and object subfields for an en-tagged post."
  (let* ((post (objects-fixture-post))
         (obj (site.activitypub:prepare-fedi-object post "Create")))
    (is (equal "Create" (cdr (assoc "type" obj :test #'string=))))
    (is (equal "https://rayslava.com/ap/actor/blog"
               (cdr (assoc "actor" obj :test #'string=))))
    (is (search "blog?id=3700000000"
                (cdr (assoc "id" obj :test #'string=))))
    ;; @context gets the language wrapper for en.
    (let ((ctx (cdr (assoc "@context" obj :test #'string=))))
      (is-true (find "en" (flatten-tree ctx) :test #'equal)))
    (let ((object (cdr (assoc "object" obj :test #'string=))))
      (is (equal "Note" (cdr (assoc "type" object :test #'string=))))
      (is (equal "https://www.w3.org/ns/activitystreams#Public"
                 (cdr (assoc "to" object :test #'string=))))
      (is (search "hello, world"
                  (cdr (assoc "content" object :test #'string=)))))))

(defun flatten-tree (x)
  "Collect all atoms in a nested cons structure — used so we can poke
inside @context which mixes strings and alists."
  (cond ((null x) nil)
        ((atom x) (list x))
        (t (append (flatten-tree (car x)) (flatten-tree (cdr x))))))

(test ap-objects-prepare-fedi-object-untagged
  "A post without en/ru tags omits the @language alist entry."
  (let* ((post (objects-fixture-post :tags '("note")))
         (obj (site.activitypub:prepare-fedi-object post "Create")))
    ;; @context should be the bare activitystreams URI, not a pair.
    (is (equal "https://www.w3.org/ns/activitystreams"
               (cdr (assoc "@context" obj :test #'string=))))))

(test ap-objects-fedi-post-create-is-json
  "fedi-post-create returns a string and decodes back to an alist."
  (let* ((post (objects-fixture-post))
         (json (site.activitypub:fedi-post-create post))
         (decoded (cl-json:decode-json-from-string json)))
    (is (stringp json))
    (is-true (assoc :type decoded))
    (is (equal "Create" (cdr (assoc :type decoded))))))

(test ap-objects-fedi-note-create-produces-note
  "fedi-note-create wraps the post as a standalone Note (not a Create)."
  (let* ((post (objects-fixture-post :tags '("ru")))
         (json (site.activitypub:fedi-note-create post))
         (decoded (cl-json:decode-json-from-string json)))
    (is (equal "Note" (cdr (assoc :type decoded))))
    ;; Russian posts should include a contentMap.
    (is-true (assoc :content-map decoded))))

(test ap-objects-fedi-post-update-marks-update-type
  "fedi-post-update emits a Create-shaped activity but with type Update."
  (let* ((post (objects-fixture-post))
         (json (site.activitypub:fedi-post-update post))
         (decoded (cl-json:decode-json-from-string json)))
    (is (equal "Update" (cdr (assoc :type decoded))))))

(test ap-objects-prepare-image-attachments
  "Image attachments become a list of ActivityStreams Document entries."
  (let* ((atts (list (make-instance 'site.blog-post:blog-post-attachment
                                    :att-type 'site.blog-post:image
                                    :url "https://example.com/a.jpg")
                     (make-instance 'site.blog-post:blog-post-attachment
                                    :att-type 'site.blog-post:image
                                    :url "https://example.com/b.jpg")))
         (pair (site.activitypub:prepare-image-attachments atts)))
    (is (equal "attachment" (car pair)))
    (is (= 2 (length (cdr pair))))
    (is (equal "Document" (cdr (assoc "type" (first (cdr pair)) :test #'string=))))
    (is (equal "https://example.com/a.jpg"
               (cdr (assoc "url" (first (cdr pair)) :test #'string=))))))

(test ap-objects-content-newlines-collapsed
  "Newlines in the post body are collapsed to spaces in the Note content."
  (let* ((post (objects-fixture-post :body (format nil "<p>line1~%  line2</p>")))
         (obj (site.activitypub:prepare-fedi-object post "Create"))
         (content (cdr (assoc "content"
                              (cdr (assoc "object" obj :test #'string=))
                              :test #'string=))))
    (is-false (search (string #\Newline) content))))
