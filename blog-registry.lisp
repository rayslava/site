;;; Blog post registry.
;;;
;;; Owns the live list of blog-post instances and exposes lookup/filter
;;; operations. Lives in its own package so that both `site.blog` (HTTP
;;; rendering) and `site.activitypub` (federation) can read post data without
;;; depending on each other.

(defpackage :site.blog-registry
  (:use :cl :site.blog-post)
  (:export :register-post
           :unregister-post
           :clear-posts
           :all-posts
           :find-post-by-id
           :posts-by-tags
           :split-by-comma))

(in-package :site.blog-registry)

(defvar *blog-posts* '()
  "Sorted list of all registered blog-post instances. Sort key: ascending id.")

(defun clear-posts ()
  "Drop every registered post. Useful on module reload and in tests."
  (setf *blog-posts* '())
  (values))

(defun register-post (post)
  "Insert POST, replacing any existing entry with the same id. Returns POST."
  (setf *blog-posts*
        (merge 'list
               (remove-if (lambda (p) (eql (id p) (id post))) *blog-posts*)
               (list post)
               #'less))
  post)

(defun unregister-post (id)
  "Remove the post with the given ID. Returns T if one was removed."
  (let ((before (length *blog-posts*)))
    (setf *blog-posts* (remove-if (lambda (p) (eql (id p) id)) *blog-posts*))
    (< (length *blog-posts*) before)))

(defun all-posts ()
  "Return the current registered list. Safe to read; do not mutate."
  *blog-posts*)

(defun find-post-by-id (id)
  "Return the post with matching ID, or NIL."
  (find id *blog-posts* :key #'id))

(defun split-by-comma (string)
  "Split STRING on commas, trimming whitespace around each piece."
  (loop for i = 0 then (1+ j)
        as j = (position #\Comma string :start i)
        collect (string-trim '(#\Space #\Tab #\Newline) (subseq string i j))
        while j))

(defun posts-by-tags (tags)
  "Return posts containing every tag in TAGS (a comma-separated string)."
  (let ((taglist (split-by-comma tags)))
    (remove-if (lambda (post)
                 (set-difference taglist (tags post) :test #'equal))
               *blog-posts*)))
