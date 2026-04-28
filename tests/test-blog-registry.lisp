(in-package :site.tests)

(in-suite all-tests)

(defun make-test-post (id subject &key (tags '()))
  (make-instance 'site.blog-post:blog-post
                 :id id
                 :subject subject
                 :tags tags
                 :post (lambda () "body")))

(defmacro with-empty-registry (&body body)
  "Snapshot the global post list, clear it for the test, restore afterwards."
  `(let ((saved (site.blog-registry:all-posts)))
     (unwind-protect
          (progn
            (site.blog-registry:clear-posts)
            ,@body)
       (setf site.blog-registry::*blog-posts* saved))))

(test registry-register-and-all
  "register-post should insert posts and all-posts should return them sorted by id."
  (with-empty-registry
    (site.blog-registry:register-post (make-test-post 200 "second"))
    (site.blog-registry:register-post (make-test-post 100 "first"))
    (let ((ids (mapcar #'site.blog-post:id (site.blog-registry:all-posts))))
      (is (equal '(100 200) ids)))))

(test registry-register-replaces-by-id
  "Registering a post with an existing id should replace, not duplicate."
  (with-empty-registry
    (site.blog-registry:register-post (make-test-post 42 "original"))
    (site.blog-registry:register-post (make-test-post 42 "updated"))
    (is (= 1 (length (site.blog-registry:all-posts))))
    (is (string= "updated"
                 (site.blog-post:subject (site.blog-registry:find-post-by-id 42))))))

(test registry-unregister-post
  "unregister-post removes by id and reports whether anything was removed."
  (with-empty-registry
    (site.blog-registry:register-post (make-test-post 1 "a"))
    (site.blog-registry:register-post (make-test-post 2 "b"))
    (is-true (site.blog-registry:unregister-post 1))
    (is (= 1 (length (site.blog-registry:all-posts))))
    (is-false (site.blog-registry:unregister-post 999))))

(test registry-find-post-by-id
  "find-post-by-id returns the matching post or nil."
  (with-empty-registry
    (site.blog-registry:register-post (make-test-post 7 "lucky"))
    (is (string= "lucky"
                 (site.blog-post:subject (site.blog-registry:find-post-by-id 7))))
    (is-false (site.blog-registry:find-post-by-id 8))))

(test registry-posts-by-tags-all-match
  "posts-by-tags returns posts whose tag set is a superset of the requested tags."
  (with-empty-registry
    (site.blog-registry:register-post
     (make-test-post 1 "en-only" :tags '("en")))
    (site.blog-registry:register-post
     (make-test-post 2 "ru-only" :tags '("ru")))
    (site.blog-registry:register-post
     (make-test-post 3 "en-fedi" :tags '("en" "fedi")))
    (let ((subs (mapcar #'site.blog-post:subject
                        (site.blog-registry:posts-by-tags "en,fedi"))))
      (is (equal '("en-fedi") subs)))))

(test registry-split-by-comma
  "split-by-comma trims whitespace around each piece."
  (is (equal '("a" "b" "c")
             (site.blog-registry:split-by-comma "a, b ,c")))
  (is (equal '("")
             (site.blog-registry:split-by-comma ""))))

(test registry-clear-posts
  "clear-posts wipes the global list."
  (with-empty-registry
    (site.blog-registry:register-post (make-test-post 1 "a"))
    (site.blog-registry:clear-posts)
    (is (null (site.blog-registry:all-posts)))))
