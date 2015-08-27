;;; A personal blog engine main file
(defpackage :piserv.blog
  (:use :cl :hunchentoot :cl-who :ht-simple-ajax
	:asdf :piserv))

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
  (format output "Blog post ID:~d" (id post)))

(defmethod less ((fst blog-post) (snd blog-post))
  (< (id fst) (id snd)))

(defmacro defblogpost (id subject post &optional tags)
  "Create new blog post inside blog-posts with ID, POST and TAGS"
  `(setf blog-posts
	 (merge 'list
		blog-posts
		(list (make-instance 'blog-post :id ,id
				     :subject subject
				     :post (lambda ()
					     (with-html-output-to-string (*standard-output* nil :prologue nil)
					       (:div :class "blog-post"
						     ,post)))
				     ,@(when tags `(:tags ,tags))))
		#'less)))
