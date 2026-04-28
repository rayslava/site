(in-package :site.tests)

(in-suite all-tests)

(defun actor-fixture-post (id subject tags body)
  (make-instance 'site.blog-post:blog-post
                 :id id
                 :subject subject
                 :tags tags
                 :post (let ((s body)) (lambda () s))))

(test ap-actor-generate-outbox-collection-empty
  "With no fedi-tagged posts, the outbox reports zero items."
  (with-empty-registry
    (let ((col (site.activitypub:generate-outbox-collection 1)))
      (is (= 0 (cdr (assoc "totalItems" col :test #'string=))))
      (is (equal "OrderedCollection"
                 (cdr (assoc "type" col :test #'string=)))))))

(test ap-actor-generate-outbox-collection-lists-fedi-posts
  "Fedi-tagged posts appear in orderedItems on page 1; non-fedi posts
are filtered out."
  (with-empty-registry
    (site.blog-registry:register-post
     (actor-fixture-post 3723894000 "fedi-1" '("en" "fedi") "<p>a</p>"))
    (site.blog-registry:register-post
     (actor-fixture-post 3723895000 "plain" '("en") "<p>b</p>"))
    (site.blog-registry:register-post
     (actor-fixture-post 3723896000 "fedi-2" '("en" "fedi") "<p>c</p>"))
    (let* ((col (site.activitypub:generate-outbox-collection 1))
           (items (cdr (assoc "orderedItems" col :test #'string=))))
      (is (= 2 (cdr (assoc "totalItems" col :test #'string=))))
      (is (= 2 (length items))))))

(test ap-actor-generate-outbox-collection-pagination
  "Page 2 URL pair appears only when totalItems exceeds one page."
  (with-empty-registry
    ;; Register 25 fedi posts to overflow items-per-page (20).
    (dotimes (i 25)
      (site.blog-registry:register-post
       (actor-fixture-post (+ 3700000000 i)
                           (format nil "post-~D" i)
                           '("fedi")
                           "<p>x</p>")))
    (let ((page1 (site.activitypub:generate-outbox-collection 1))
          (page2 (site.activitypub:generate-outbox-collection 2)))
      (is (= 25 (cdr (assoc "totalItems" page1 :test #'string=))))
      (is-true (assoc "next" page1 :test #'string=))
      (is-false (assoc "prev" page1 :test #'string=))
      (is-true (assoc "prev" page2 :test #'string=))
      (is-false (assoc "next" page2 :test #'string=))
      (is (= 5 (length (cdr (assoc "orderedItems" page2 :test #'string=))))))))

(test ap-actor-generate-accept-wraps-request
  "generate-accept returns a JSON string whose type is Accept and whose
object is the original Follow request."
  (let* ((follow '(("type" . "Follow")
                   ("actor" . "https://alice.example/actor")))
         (json (site.activitypub:generate-accept follow))
         (decoded (cl-json:decode-json-from-string json)))
    (is (equal "Accept" (cdr (assoc :type decoded))))
    (is (equal "https://rayslava.com/ap/actor/blog"
               (cdr (assoc :actor decoded))))
    (let ((object (cdr (assoc :object decoded))))
      (is (equal "Follow" (cdr (assoc :type object)))))))
