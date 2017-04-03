(defpackage #:site-asd
  (:use :cl :asdf))

(in-package :site-asd)

(defsystem :site
  :name "site"
  :version "0.1"
  :maintainer "rayslava"
  :author "rayslava"
  :licence "BSD"
  :description "The homepage server"
  :long-description "Lisp implementation of my home page"
  :depends-on (:hunchentoot :cl-who :ht-simple-ajax :cl-css :local-time :dyna :cl-json)
  :components ((:file "site"
                      :depends-on ("static" "config"))
               (:file "static"
		      :depends-on ("config"))
	       (:file "config")
	       (:file "blog")
	       (:file "pages"
		      :depends-on ("site" "blog"))
	       (:file "style"
		      :depends-on ("site"))
	       (:file "blogposts"
		      :depends-on ("blog"))
	       (:file "db-storage"
		      :depends-on ("config"))))
