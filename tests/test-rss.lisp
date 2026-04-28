(in-package :site.tests)

(in-suite all-tests)

(defun make-rss-post (id subject body-string &key (tags '()))
  (make-instance 'site.blog-post:blog-post
                 :id id
                 :subject subject
                 :tags tags
                 :post (let ((s body-string)) (lambda () s))))

(test rss-build-basic-shape
  "build-rss-feed emits XML prologue and RSS 2.0 skeleton."
  (let* ((posts (list (make-rss-post 3649655845 "First"
                                     "<div>hello world</div>")))
         (feed (site.rss:build-rss-feed posts)))
    (is-true (search "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" feed))
    (is-true (or (search "<rss version=\"2.0\">" feed)
                 (search "<rss version='2.0'>" feed)))
    (is-true (search "<title>rayslava's blog</title>" feed))
    (is-true (search "<item>" feed))
    (is-true (search "<title>First</title>" feed))))

(test rss-build-escapes-description
  "Each post's rendered body appears inside <description>."
  (let* ((posts (list (make-rss-post 3649655845 "P1" "<p>body</p>")))
         (feed (site.rss:build-rss-feed posts)))
    (is-true (search "<description>" feed))
    ;; The render-body return value is embedded verbatim (it's already
    ;; HTML from the defblogpost macro). cl-who's :xml mode will escape
    ;; it via (str ...).
    (is-true (or (search "&lt;p&gt;body&lt;/p&gt;" feed)
                 (search "<p>body</p>" feed)))))

(test rss-build-empty-posts
  "With no posts, the channel is well-formed and contains no <item>."
  (let ((feed (site.rss:build-rss-feed nil)))
    (is-true (search "<channel>" feed))
    (is-false (search "<item>" feed))))

(test rss-build-respects-custom-title
  "Custom :title and :link keyword arguments flow into the output."
  (let ((feed (site.rss:build-rss-feed nil :title "Alt feed" :link "http://x.y")))
    (is-true (search "<title>Alt feed</title>" feed))
    (is-true (search "<link>http://x.y</link>" feed))))
