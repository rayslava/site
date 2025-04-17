;;; A personal blog engine main file
(defpackage :site.blog
  (:use :cl :hunchentoot :cl-who :ht-simple-ajax
	:asdf :site :local-time :site.activitypub :site.blog-post)
  (:export :defblogpost))

(in-package :site.blog)

(defvar *blog-posts* '() "List of all blog posts sorted by ID")

(defmacro defblogpost (id subject post &key meta tags attachment)
  "Create new blog post inside *blog-posts* with ID, POST and TAGS"
  `(setf *blog-posts*
	 (merge 'list
		*blog-posts*
		(list (make-instance 'blog-post
				     :id ,id
				     :subject ,subject
				     :post (lambda ()
					     (with-html-output-to-string (*standard-output* nil :prologue nil)
					       (:div :class "blog-post"
						     ,post)))
				     ,@(when meta `(:meta
						    (lambda ()
						      (with-html-output-to-string (*standard-output* nil) (htm ,@meta)))))
				     ,@(when attachment `(:attachment
							  (make-instance 'blog-post-attachment
									 :att-type ,(getf attachment :type)
									 :url ,(getf attachment :url))))
				     ,@(when tags `(:tags ,tags)))) #'less)))


(setf (html-mode) :html5)

;;; Creating class system to use for blog posts

(defun split-by-comma (string)
  "Returns a list of substrings of string
divided by comma each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
	as j = (position #\Comma string :start i)
	collect (string-trim '(#\Space #\Tab #\Newline) (subseq string i j))
	while j))

(defun posts-by-tags (tags)
  "Returns only posts containing tags
TAGS is comma-separated string"
  (let ((taglist (split-by-comma tags)))
    (remove-if (lambda (post)
		 (set-difference taglist (tags post) :test #'equal))
	       *blog-posts*)))

(defun blog-page-head ()
  "Common head part for all blog pages"
  (with-html-output-to-string (*standard-output* nil :prologue nil)
    (:link :rel "stylesheet" :type "text/css" :href "/main.css")
    (:link :rel "stylesheet" :type "text/css" :href "/blog.css")
    (:link :rel "alternate"  :type "application/rss+xml" :title "rayslava" :href "/rss")
    (:meta :http-equiv "Content-Type" :content "text/html; charset=utf-8")
    (:meta :name "viewport" :content "initial-scale=1.0,maximum-scale=1.0,width=device-width,user-scalable=0")))

(defun show-fedi-comments (id)
  (let ((replies (get-all-replies id)))
    (when replies
      (with-html-output-to-string (*standard-output* nil :prologue nil)
	(htm (:div
	      :class "comments"
	      (dolist (reply replies)
		(when (cdr (assoc :public reply))
		  (htm (:div
			:class "apub-reply"
			(:span :class "commenter" (format t "~a" (cdr (assoc :actor reply))))
			(:span :class "comment" (format t "~a" (cdr (assoc :content reply))))
			(:span :class "comment-time" (:a :href (format nil "~a" (cdr (assoc :url reply)))
							 (format-timestring t (universal-to-timestamp (cdr (assoc :published reply)))
									    :format '((:hour 2) ":" (:min 2) " " (:year 4) "-" (:month 2) "-" (:day 2) " " :gmt-offset-hhmm))))))))))))))

(defun show-post (id)
  (let* ((post (car (member-if (lambda (e) (eql (id e) id)) *blog-posts*)))
	 (subject (subject post))
	 (taglist (tags post))
	 (text (post post))
	 (meta (meta post))
	 (att (if (slot-boundp post 'attachment)
		  (attachment post)
		  nil))
	 (timestamp (universal-to-timestamp (id post)))
	 (datetime-tag (format-timestring nil timestamp :format '((:year 4) "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2) :gmt-offset-hhmm)))
	 (posted-at (format-timestring nil timestamp :format '((:hour 2) ":" (:min 2) " " (:year 4) "-" (:month 2) "-" (:day 2) " " :gmt-offset-hhmm))))
    (with-html-output-to-string (*standard-output* nil :prologue nil)
      (htm (:html
	    (:head (:title (format t "~a" subject)subject)

		   (format t "~a" (blog-page-head))
		   (when meta
		     (format t "~a" (funcall meta))))
	    (htm (:body
		  (:article
		   (:h2 (format t "~a" subject))
		   (format t "~a" (funcall text))
		   (when att
		     (htm
		      (:div
		       :id "attachment"
		       (cond ((eq (slot-value att 'att-type) 'image)
			      (htm (:img :src (url att))))
			     (t)))))
		   (htm (:div
			 :id "postinfo"
			 (:nav
			  (:span :id "taglist"
				 (dolist (tag taglist)
				   (if (string-equal tag "fedi")
				       (htm (:a :href (format nil "/blog?tags=~a" tag)
						(:span :class "tag"
						       (format t "&#x2B50;~a &#x1F501;~a"
							       (reactions-number id "Like")
							       (reactions-number id "Announce")))))
				       (htm (:a :href (format nil "/blog?tags=~a" tag)
						(:span :class "tag"
						       (format t "~a" tag)))))))))
			(:span :id "timeinfo"
			       (:time
				:datetime (format nil "~a" datetime-tag)
				(format t "~a" posted-at))))
		   (when (member "fedi" taglist :test #'string-equal)
		     (format t "~a" (or (show-fedi-comments id) "")))))))))))

(defun list-posts (tags)
  (let* ((postlist (sort (copy-list
                          (if tags
                              (posts-by-tags tags)
                              *blog-posts*))
                         #'> :key #'id))
         (current-year nil))
    (with-html-output-to-string (*standard-output* nil :prologue nil)
      (htm (:html
            (:head (:title "Blog")
                   (format t "~a" (blog-page-head))
                   (:body
                    (:div :class "posts-by-year"
                          (dolist (post postlist)
                            (let ((post-year (local-time:timestamp-year
                                              (local-time:universal-to-timestamp (id post)))))
                              (when (not (eql post-year current-year))
                                (setf current-year post-year)
                                (htm (:div :class "year-split"
					   (:h3 (format t "~A" post-year)))))
                              (htm (:ul :class "text-with-image"
                                        (:li (:a :href (format nil "/blog?id=~a" (id post))
                                                 (format t "~a~a"
                                                         (if (member "fedi" (tags post) :test #'string=)
							     (with-html-output-to-string (s)
                                                               (:span (:img :src "https://rayslava.com/i/apub.svg" :alt "apub")))
							     "")
                                                         (subject post)))))))))
		    (:div :class "blog-stats"
			  (:p (format t "Blog posts: ~d" (length postlist)))
                    (:p :class "text-with-image"
                        (:p
                            "The line marked with "
                            (:span (:img :height "24px;" :src "https://rayslava.com/i/apub.svg" :alt "apub"))
                            " are published via ActivityPub as well"))))))))))

(define-easy-handler (blog-page :uri "/blog"
				:default-request-type :get)
    ((id :parameter-type 'integer)
     (tags :parameter-type 'string))
  (if (and id
	   (let ((accept-header (header-in :accept *request*)))
	     (when (stringp accept-header)
	       (some (lambda (target-type)
		       (search target-type accept-header :test #'string-equal))
		     '("application/ld+json; profile=\"http://www.w3.org/ns/activitystreams\""
		       "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\""
		       "application/activity+json")))))
      (progn
	(setf (hunchentoot:content-type*) (header-in :accept *request*))
	(fedi-note-create (car (member-if (lambda (e) (eql (id e) id)) *blog-posts*))))
      (with-html-output-to-string (*standard-output* nil :prologue t)
	(format t "~a"
		(if id
		    (show-post id)
		    (list-posts tags))))))

;;; The RSS feed
(define-easy-handler (rss-page :uri "/rss"
			       :default-request-type :get)
    ()
  (let ((cl-who::*empty-tag-end* :xml)
	(cl-who::*empty-attribute-syntax* nil))
    (cl-who:with-html-output-to-string (s nil :prologue "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" :indent nil)
      (:rss :|version| "2.0"
	    (:channel
	     (:title "rayslava's blog")
	     (:link "http://rayslava.com")
	     (:description "Blog feed")
	     (dolist (post *blog-posts*)
	       (let ((title (subject post))
		     (link (format nil "http://rayslava.com/blog?id=~a" (id post)))
		     (description (post post)))
		 (cl-who:htm (:item
			      (cl-who:htm (:title (cl-who:str title))
					  (:link (cl-who:str link))
					  (:pubDate (cl-who:str (format-timestring nil (universal-to-timestamp (id post)) :format +rfc-1123-format+)))
					  (:description (cl-who:str (funcall description)))))))))))))
