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
	       :ironclad :asn1 :trivia :cl-base64 :uuid :quri :cl-ppcre
	       :cl-json-helper :trivial-gray-streams :bordeaux-threads
	       :local-time :slynk)
  :in-order-to ((test-op (test-op :site/tests)))
  :components ((:file "log"
		:depends-on ("db-manage" "config"))
	       (:file "site"
                :depends-on ("static" "config" "log" "blog-registry" "activitypub" "db-storage"))
               (:file "static"
		:depends-on ("config" "db-manage"))
	       (:file "config")
	       (:file "blog-post")
	       (:file "blog-registry"
		:depends-on ("blog-post"))
	       (:file "rss"
		:depends-on ("blog-post" "blog-registry"))
	       (:file "blog"
		:depends-on ("activitypub" "blog-post" "blog-registry" "rss"))
	       (:file "pages"
		:depends-on ("site" "blog" "db-manage"))
	       (:file "style"
		:depends-on ("site"))
	       (:file "blogposts"
		:depends-on ("blog" "activitypub" "blog-registry"))
	       (:file "lj"
		:depends-on ("blogposts"))
	       (:file "db-storage"
		:depends-on ("config"))
	       (:file "db-manage"
		:depends-on ("db-storage"))
	       (:file "crypto")
	       (:file "ap-signature"
		:depends-on ("crypto"))
	       (:file "storage")
	       (:file "activitypub"
		:depends-on ("config" "crypto" "ap-signature" "blog-post" "blog-registry" "storage"))))

(defsystem :site/tests
  :name "site/tests"
  :description "Test suite for site"
  :depends-on (:site :fiveam)
  :pathname "tests"
  :components ((:file "package")
               (:file "main" :depends-on ("package"))
               (:file "test-crypto" :depends-on ("package"))
               (:file "test-blog-registry" :depends-on ("package"))
               (:file "test-storage-memory" :depends-on ("package"))
               (:file "test-ap-signature" :depends-on ("package" "test-crypto"))
               (:file "test-rss" :depends-on ("package")))
  :perform (test-op (op c)
             (uiop:symbol-call :fiveam :run!
                               (uiop:find-symbol* :all-tests :site.tests))))
