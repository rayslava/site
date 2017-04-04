(defpackage :site.static
  (:use :cl :cl-who :hunchentoot :site.config)
  (:export :generate-static-table))

(in-package :site.static)

(defun generate-files-list (path)
  "Prepares a list of files in directory"
  (directory
   (make-pathname :directory path
		  :type :wild
		  :name :wild
		  :version :wild)))

(defun get-file-name-type (file)
  "Gets file name.type from pathspec"
  (concatenate 'string
	       (pathname-name file)
	       "."
	       (pathname-type file)))

(defun generate-static-table ()
  (loop for element in (generate-files-list *default-static-path*)
     collect (hunchentoot:create-static-file-dispatcher-and-handler
	      (concatenate 'string "/" (get-file-name-type element))
	      element)))
