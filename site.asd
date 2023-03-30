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
  :depends-on (:hunchentoot :cl-who :ht-simple-ajax :cl-css
	       :cl-json :local-time :dyna :zs3 :jonathan :trivial-mimes
	       :ironclad :asn1 :trivia :cl-base64 :uuid :quri :cl-ppcre)
  :components ((:file "site"
                :depends-on ("static" "config"))
               (:file "static"
		:depends-on ("config" "db-manage"))
	       (:file "config")
	       (:file "blog-post")
	       (:file "blog"
		:depends-on ("activitypub" "blog-post"))
	       (:file "pages"
		:depends-on ("site" "blog" "db-manage"))
	       (:file "style"
		:depends-on ("site"))
	       (:file "blogposts"
		:depends-on ("blog" "activitypub"))
	       (:file "lj"
		:depends-on ("blogposts"))
	       (:file "db-storage"
		:depends-on ("config"))
	       (:file "db-manage"
		:depends-on ("db-storage"))
	       (:file "crypto")
	       (:file "activitypub"
		:depends-on ("config" "crypto" "blog-post"))))
