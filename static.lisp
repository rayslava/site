(defpackage :piserv.static
  (:use :cl :cl-who :hunchentoot)
  (:export :generate-static-table))

(in-package :piserv.static)

(defparameter default-static-path 
  (directory
   (make-pathname
    :version nil :defaults
    (merge-pathnames
     (make-pathname :directory '(:relative "static")
		    :type :wild
		    :name :wild
		    :version :wild)
     (load-time-value
      (or *load-pathname* *default-pathname-defaults*)))))
  "Default path where server should search for files that should be exported as is")

(defun generate-files-list (path)
  "Prepares a list of files in directory"
  (loop for file in (directory path)
	 collect file))

(defun get-file-name-type (file)
  "Gets file name.type from pathspec"
  (concatenate 'string
	  (pathname-name file)
	  "."
	  (pathname-type file)))

(defun generate-static-table ()
  (loop for element in default-static-path
     collect (hunchentoot:create-static-file-dispatcher-and-handler
	      (concatenate 'string "/" (get-file-name-type element))
	      element)))
