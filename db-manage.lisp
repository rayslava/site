(defpackage :site.db-manage
  (:use :cl :site :site.config :site.db-storage :hunchentoot :cl-who :jonathan))

(in-package :site.db-manage)
(setf (html-mode) :html5)

(defun format-link (name)
  "Generate the s3 link to required `name'"
  (concatenate 'string "https://" zs3:*s3-endpoint* "/" *static-bucket* "/" name))

(defun beautify-attrs (attrs)
  "Parse the attrs of `attrs' and print the beautiful HTML"
  (let ((attrlist (parse attrs)))
    (with-html-output-to-string (*standard-output* nil :prologue nil)
      (:b "Tags: ")
      (mapcar #'(lambda (e) (fmt "~a " e))
	      (getf attrlist :tags)))))

(define-easy-handler (staticlist :uri "/staticlist"
				 :default-request-type :get)
    ()
  (with-http-authentication
      (with-html-output-to-string (*standard-output* nil :prologue t)
	(:html
	 (:head (:title "List of available static files")
		(:link :rel "stylesheet" :type "text/css" :href "/main.css")
		(:script :type "text/javascript" :src "/jscl.js"))
	 (:body (:h2 "Statics")
		(:table
		 (:tr (:td "Filename") (:td "Attrs"))
		 (mapcar (lambda (e)
			   (htm (:tr (:td
				      (:a :href (format nil "~A" (format-link (filename e)))
					  (fmt "~A" (filename e))))
				     (:td (fmt "~A" (beautify-attrs (attr e)))))))
			 (site.db-storage:list-available-statics))))))))
