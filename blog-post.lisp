;;; A personal blog engine main file
(defpackage :site.blog-post
  (:use :cl :asdf :site)
  (:export :blog-post :att-type :url :defblogpost :id :tags :post :attachment
	   :less :print-object :subject :meta :attachment-type
   :blog-post-attachment :image))

(in-package :site.blog-post)

(deftype attachment-type ()
  "ATTACHMENT-TYPE defines the types of attachments for blog posts supported by
 the engine"
  '(member image))

(defclass blog-post-attachment ()
  ((att-type :accessor att-type
	     :initarg :att-type
	     :type attachment-type
	     :initform (error "Must supply an ATTACHMENT-TYPE")
	     :documentation "Attachment type. Currently images are supported")
   (url :accessor url
	:initarg :url
	:initform (error "Must supply an URL")
	:documentation "Url leading to attachment")))

(defmethod print-object ((att blog-post-attachment) (output stream))
  (format output "Blog attachment from: ~a" (url att)))

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
   (attachment :accessor attachment
	       :initarg :attachment
	       :type blog-post-attachment
	       :documentation "Attachment data to follow the html and to be attached to ActivityPub post.")
   (post :accessor post
	 :initarg :post
	 :initform (error "Must supply a POST")
	 :documentation "Function which returns a post")))

(defmethod print-object ((post blog-post) (output stream))
  (format output "Blog post ID:~d (~a)" (id post) (tags post)))

(defmethod less ((fst blog-post) (snd blog-post))
  (< (id fst) (id snd)))
