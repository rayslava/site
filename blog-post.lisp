;;; A personal blog engine main file
(defpackage :site.blog-post
  (:use :cl :asdf :site)
  (:export :blog-post :defblogpost :id :tags :post :*blog-posts*))

(in-package :site.blog-post)

(defvar *blog-posts* '() "List of all blog posts sorted by ID")

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
   (meta :accessor meta
	 :initarg :meta
	 :initform '()
	 :documentation "Metadata to be embedded into head of html. A cl-who S-form.")
   (post :accessor post
	 :initarg :post
	 :initform (error "Must supply a POST")
	 :documentation "Function which returns a post")))

(defmethod print-object ((post blog-post) (output stream))
  (format output "Blog post ID:~d (~a)" (id post) (tags post)))

(defmethod less ((fst blog-post) (snd blog-post))
  (< (id fst) (id snd)))

(defmacro defblogpost (id subject post &key meta tags)
  "Create new blog post inside *blog-posts* with ID, POST and TAGS"
  `(setf *blog-posts*
	 (merge 'list
		*blog-posts*
		(list (make-instance 'blog-post :id ,id
						:subject ,subject
						:post (lambda ()
							(with-html-output-to-string (*standard-output* nil :prologue nil)
							  (:div :class "blog-post"
								,post)))
						,@(when meta `(:meta
							       (lambda ()
								 (with-html-output-to-string (*standard-output* nil) (htm ,@meta)))))
						,@(when tags `(:tags ,tags)))) #'less)))

