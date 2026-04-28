(in-package :site.tests)

(in-suite all-tests)

(defun make-rss-post (id subject body-string &key (tags '()))
  (make-instance 'site.blog-post:blog-post
                 :id id
                 :subject subject
                 :tags tags
                 :post (let ((s body-string)) (lambda () s))))

(test rss-build-basic-shape
  "build-rss-feed emits XML prologue, RSS 2.0 skeleton, and an item
title. The channel title contains an apostrophe which must be
entity-escaped."
  (let* ((posts (list (make-rss-post 3649655845 "First"
                                     "<div>hello world</div>")))
         (feed (site.rss:build-rss-feed posts)))
    (is-true (search "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" feed))
    (is-true (or (search "<rss version=\"2.0\">" feed)
                 (search "<rss version='2.0'>" feed)))
    ;; rayslava's apostrophe may be encoded as &apos; or &#039; depending
    ;; on the XML emitter; both are valid XML.
    (is-true (or (search "rayslava&apos;s blog" feed)
                 (search "rayslava&#039;s blog" feed)))
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

(defun rss-well-formed-p (xml)
  "Parse XML through cxml with a null SAX handler; returns T on well-formed
XML and NIL on any parse error. cxml is strict — unescaped ampersands,
unclosed tags, and stray void elements all fail here."
  (handler-case (progn (cxml:parse xml nil) t)
    (error () nil)))

(test rss-output-is-well-formed-xml
  "The feed must be parseable as XML — no unescaped ampersands or raw
HTML5 void tags inside <description>."
  (let* ((post (make-instance 'site.blog-post:blog-post
                              :id 3700000000
                              :subject "Hello & world"
                              :tags '("en")
                              :post (lambda ()
                                      "<div class=\"blog-post\"><p>line<br>with void <img src=\"/x\"></p></div>")))
         (feed (site.rss:build-rss-feed (list post))))
    (is-true (rss-well-formed-p feed)
             "build-rss-feed produced non-well-formed XML:~%~A" feed)))

(test rss-escapes-ampersand-in-title
  "An ampersand in the title appears as &amp; in the rendered feed."
  (let* ((post (make-instance 'site.blog-post:blog-post
                              :id 3700000000
                              :subject "R&D"
                              :tags '("en")
                              :post (lambda () "<p>body</p>")))
         (feed (site.rss:build-rss-feed (list post))))
    (is-true (search "R&amp;D" feed)
             "expected entity-escaped ampersand in title; got: ~A" feed)
    (is-false (search "R&D" feed))))

(test rss-description-wraps-html-in-cdata
  "The HTML body is wrapped in <![CDATA[ ... ]]> so that void tags like
<br> and <img> do not break XML well-formedness."
  (let* ((post (make-instance 'site.blog-post:blog-post
                              :id 3700000000
                              :subject "x"
                              :tags '("en")
                              :post (lambda () "<p>line<br>more</p>")))
         (feed (site.rss:build-rss-feed (list post))))
    (is-true (search "<![CDATA[" feed)
             "description must be CDATA-wrapped; got: ~A" feed)
    (is-true (search "]]>" feed))))

(test rss-pubdate-preserves-camelcase
  "<pubDate> (not <pubdate>) — canonical RSS 2.0 casing."
  (let* ((post (make-instance 'site.blog-post:blog-post
                              :id 3700000000
                              :subject "x"
                              :tags '("en")
                              :post (lambda () "<p>b</p>")))
         (feed (site.rss:build-rss-feed (list post))))
    (is-true (search "<pubDate>" feed)
             "expected <pubDate> with canonical camelCase; got: ~A" feed)))

(test rss-cdata-safe-against-end-sequence
  "If the post body contains the literal ]]> it must not terminate our
CDATA section prematurely. Standard trick: split into two CDATA
sections across the ]]>."
  (let* ((post (make-instance 'site.blog-post:blog-post
                              :id 3700000000
                              :subject "x"
                              :tags '("en")
                              :post (lambda () "<p>code: x ]]> y</p>")))
         (feed (site.rss:build-rss-feed (list post))))
    (is-true (rss-well-formed-p feed)
             "feed with ]]> in body must stay well-formed; got: ~A" feed)))
