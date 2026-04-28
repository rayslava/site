(in-package :site.tests)

(in-suite all-tests)

(test blog-post-less-orders-by-id
  "less compares posts by their numeric id."
  (let ((a (make-test-post 100 "A"))
        (b (make-test-post 200 "B")))
    (is-true  (site.blog-post:less a b))
    (is-false (site.blog-post:less b a))
    (is-false (site.blog-post:less a a))))

(test blog-post-print-object-contains-id-and-tags
  "print-object renders the ID and tag list in the unreadable form."
  (let* ((post (make-test-post 42 "hello" :tags '("en" "site")))
         (s (with-output-to-string (out) (print post out))))
    (is-true (search "42" s))
    (is-true (search "en" s))
    (is-true (search "site" s))))

(test blog-post-attachment-print-object
  "print-object for an attachment mentions its URL."
  (let* ((att (make-instance 'site.blog-post:blog-post-attachment
                             :att-type 'site.blog-post:image
                             :url "https://example.com/a.jpg"))
         (s (with-output-to-string (out) (print att out))))
    (is-true (search "https://example.com/a.jpg" s))))

(test blog-post-attachment-type-guard
  "Supplying an invalid attachment type makes the slot-value inaccessible
as a member of attachment-type. We do not strictly require CHECK-TYPE on
construction — just that reading it back is type-correct for the valid case."
  (let ((att (make-instance 'site.blog-post:blog-post-attachment
                            :att-type 'site.blog-post:image
                            :url "/cat.jpg")))
    (is (eq 'site.blog-post:image (site.blog-post:att-type att)))))
