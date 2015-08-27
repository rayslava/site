;;; A personal blog engine main file
(defpackage :piserv.blog
  (:use :cl :hunchentoot :cl-who :ht-simple-ajax
	:asdf :piserv)
  (:export :defblogpost))

(in-package :piserv.blog)

;;; Creating class system to use for blog posts

(defvar blog-posts '() "List of all blog posts sorted by ID")

(defclass blog-post ()
  ((id :accessor id
       :initarg :id
       :initform (error "Must supply an ID")
       :documentation "Post ID. Must be a timestamp")
   (subject :accessor subject
	    :initarg :subject
	    :initform (error "Must supply a subject")
	    :documentation "Post subject shown in list.")
   (tags :accessor tags
	 :initarg :tags
	 :initform '()
	 :documentation "Post TAGS. A list of strings")
   (post :accessor post
	 :initarg :post
	 :initform (error "Must supply a POST")
	 :documentation "Function which returns a post")))

(defmethod print-object ((post blog-post) (output stream))
  (format output "Blog post ID:~d (~a)" (id post) (tags post)))

(defmethod less ((fst blog-post) (snd blog-post))
  (< (id fst) (id snd)))

(defmacro defblogpost (id subject post &optional tags)
  "Create new blog post inside blog-posts with ID, POST and TAGS"
  `(setf blog-posts
	 (merge 'list
		blog-posts
		(list (make-instance 'blog-post :id ,id
				     :subject ,subject
				     :post (lambda ()
					     (with-html-output-to-string (*standard-output* nil :prologue nil)
					       (:div :class "blog-post"
						     ,post)))
				     ,@(when tags `(:tags ,tags))))
		#'less)))

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
	  blog-posts)))

(define-easy-handler (blog-page :uri "/blog"
				:default-request-type :get)
    ((id :parameter-type 'integer)
     (tags :parameter-type 'string))
  (with-html-output-to-string (*standard-output* nil :prologue nil)
    (:html
     (:head (:title "Blog")
	    (:link :rel "stylesheet" :type "text/css" :href "/main.css")
	    (:link :rel "stylesheet" :type "text/css" :href "/blog.css")
	    (:script :type "text/javascript" :src "/x-cl.js")
	    (:script :type "text/javascript" :src "/jscl.js")
	    (:meta :name "viewport" :content "initial-scale=1.0,maximum-scale=1.0,width=device-width,user-scalable=0"))
     (if id
	 (let* ((post (car (member-if (lambda (e) (eql (id e) id)) blog-posts)))
		(subject (subject post))
		(taglist (tags post))
		(text (post post))
		(timestamp (multiple-value-list (decode-universal-time (id post))))
		(hour (nth 2 timestamp))
		(minute (nth 1 timestamp))
		(day (nth 3 timestamp))
		(month (nth 4 timestamp))
		(year (nth 5 timestamp))
		(tz (nth 8 timestamp))
		(posted-at (format nil "~2,'0d:~2,'0d ~d-~2,'0d-~d (GMT~@d)"
				   hour
				   minute
				   year
				   month
				   day
				   (- tz))))
	   (htm (:body (:h2 (format t "~a" subject))
		       (format t "~a" (funcall text))
		       (htm (:div :id "postinfo"
				  (:span :id "taglist"
					 (dolist (tag taglist)
					   (htm (:a :href (format nil "/blog?tags=~a" tag)
						    (:span :class "tag"
							   (format t "~a" tag))))))
				  (:span :id "timeinfo"
					 (htm (format t "~a" posted-at))))))))

	 (let ((postlist (if tags
			     (posts-by-tags tags)
			     blog-posts)))
	   (htm (:body (:h2 "Blog list")
		       (:p "Posts:"
			   (:ul
			    (dolist (post postlist)
			      (htm
			       (:li (:a :href (format nil "/blog?id=~a" (id post))
					(format t "~a" (subject post)))))))))))))))
