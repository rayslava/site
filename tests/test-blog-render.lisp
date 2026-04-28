(in-package :site.tests)

(in-suite all-tests)

(defun make-renderable-post (id subject body-html &key (tags '("en")))
  "Build a blog-post whose :POST slot returns BODY-HTML verbatim — same
shape as what defblogpost produces, minus the cl-who wrapper."
  (make-instance 'site.blog-post:blog-post
                 :id id
                 :subject subject
                 :tags tags
                 :post (let ((s body-html)) (lambda () s))))

(test blog-show-post-renders-title-and-body
  "show-post produces an <article> with the post's subject and body."
  (with-empty-registry
    (let ((post (make-renderable-post 3723894000 "Hello world"
                                      "<p>hi there</p>"
                                      :tags '("en" "note"))))
      (site.blog-registry:register-post post)
      (let ((html (site.blog::show-post 3723894000)))
        (is-true (search "Hello world" html))
        (is-true (search "<p>hi there</p>" html))
        (is-true (search "<article>" html))
        ;; Tag links appear in the taglist.
        (is-true (search "/blog?tags=en" html))
        (is-true (search "/blog?tags=note" html))
        ;; Non-fedi posts must not pull in reactions or comments.
        (is-false (search "fedi" html))))))

(test blog-show-post-with-meta-renders-meta
  "If :meta is provided the renderer funcalls it and splices the output
into <head>."
  (with-empty-registry
    (let* ((meta-html "<meta property=\"og:title\" content=\"X\"/>")
           (post (make-instance 'site.blog-post:blog-post
                                :id 3700000000
                                :subject "With meta"
                                :tags '("en")
                                :post (lambda () "<p>body</p>")
                                :meta (let ((m meta-html)) (lambda () m)))))
      (site.blog-registry:register-post post)
      (let ((html (site.blog::show-post 3700000000)))
        (is-true (search "og:title" html))))))

(test blog-list-posts-groups-by-year
  "list-posts emits one year-split per distinct year."
  (with-empty-registry
    ;; Three posts: one in 2023, two in 2022 (timestamps arbitrary within).
    (dolist (args '((3723894000 "Latest"  ("en"))   ; 2023
                    (3691234000 "Middle"  ("en"))   ; 2022
                    (3689000000 "Earlier" ("en")))) ; 2022
      (destructuring-bind (id subj tags) args
        (site.blog-registry:register-post
         (make-renderable-post id subj "<p>b</p>" :tags tags))))
    (let ((html (site.blog::list-posts nil)))
      (is-true (search "year-split" html))
      (is-true (search "Latest" html))
      (is-true (search "Middle" html))
      (is-true (search "Earlier" html))
      ;; Stats block mentions total count.
      (is-true (search "Blog posts: 3" html)))))

(test blog-list-posts-filter-by-tag
  "When called with a comma-separated tag string, only matching posts appear."
  (with-empty-registry
    (site.blog-registry:register-post
     (make-renderable-post 3723894000 "EN post" "<p>b</p>" :tags '("en")))
    (site.blog-registry:register-post
     (make-renderable-post 3723895000 "RU post" "<p>b</p>" :tags '("ru")))
    (let ((html (site.blog::list-posts "en")))
      (is-true (search "EN post" html))
      (is-false (search "RU post" html))
      (is-true (search "Blog posts: 1" html)))))

(test blog-list-posts-marks-fedi-posts
  "Fedi-tagged posts get the apub.svg marker in the list."
  (with-empty-registry
    (site.blog-registry:register-post
     (make-renderable-post 3723896000 "Regular" "<p>b</p>" :tags '("en")))
    (site.blog-registry:register-post
     (make-renderable-post 3723897000 "Fedi-one" "<p>b</p>" :tags '("en" "fedi")))
    (let ((html (site.blog::list-posts nil)))
      (is-true (search "apub.svg" html)))))

(test blog-blog-page-head-includes-feed-link
  "The shared <head> fragment advertises the RSS feed and includes viewport."
  (let ((html (site.blog::blog-page-head)))
    (is-true (search "application/rss+xml" html))
    (is-true (search "/rss" html))
    (is-true (search "viewport" html))))
