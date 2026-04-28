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
                :depends-on ("static" "config" "log" "blog-registry" "ap-inbox" "db-storage"))
               (:file "static"
		:depends-on ("config" "db-manage"))
	       (:file "config")
	       (:file "blog-post")
	       (:file "blog-registry"
		:depends-on ("blog-post"))
	       (:file "rss"
		:depends-on ("blog-post" "blog-registry"))
	       (:file "blog"
		:depends-on ("ap-inbox" "blog-post" "blog-registry" "rss"))
	       (:file "pages"
		:depends-on ("site" "blog" "db-manage"))
	       (:file "style"
		:depends-on ("site"))
	       (:file "blogposts"
		:depends-on ("blog" "ap-inbox" "blog-registry"))
	       (:file "lj"
		:depends-on ("blogposts"))
	       (:file "db-storage"
		:depends-on ("config" "storage"))
	       (:file "db-manage"
		:depends-on ("db-storage" "storage"))
	       (:file "crypto")
	       (:file "ap-signature"
		:depends-on ("crypto"))
	       (:file "storage")
	       (:file "ap-core"
		:depends-on ("config" "storage" "crypto" "ap-signature"
		             "blog-post" "blog-registry" "db-manage"))
	       (:file "ap-objects"
		:depends-on ("ap-core" "blog-post"))
	       (:file "ap-replies"
		:depends-on ("ap-core" "storage"))
	       (:file "ap-delivery"
		:depends-on ("ap-core" "ap-objects" "ap-signature" "crypto" "config" "storage"))
	       (:file "ap-actor"
		:depends-on ("ap-core" "ap-objects" "config" "blog-registry"))
	       (:file "ap-inbox"
		:depends-on ("ap-core" "ap-replies" "ap-delivery" "ap-actor" "ap-signature" "crypto" "blog-registry"))
	       (:file "storage-dyna"
		:depends-on ("storage" "ap-core" "db-storage" "config"))))

(defsystem :site/tests
  :name "site/tests"
  :description "Test suite for site"
  :depends-on (:site :fiveam)
  :pathname "tests"
  :components ((:file "package")
               (:file "main" :depends-on ("package"))
               (:file "test-crypto" :depends-on ("package"))
               (:file "test-blog-registry" :depends-on ("package"))
               (:file "test-blog-post" :depends-on ("package" "test-blog-registry"))
               (:file "test-storage-memory" :depends-on ("package"))
               (:file "test-ap-signature" :depends-on ("package" "test-crypto"))
               (:file "test-ap-signature-pem" :depends-on ("package"))
               (:file "test-rss" :depends-on ("package"))
               (:file "test-config" :depends-on ("package"))
               (:file "test-blog-render" :depends-on ("package" "test-blog-registry"))
               (:file "test-ap-events" :depends-on ("package"))
               (:file "test-ap-subscribers" :depends-on ("package" "test-ap-events"))
               (:file "test-db-storage" :depends-on ("package" "test-ap-events"))
               (:file "test-ap-objects" :depends-on ("package"))
               (:file "test-ap-actor" :depends-on ("package" "test-blog-registry"))
               (:file "test-ap-inbox" :depends-on ("package" "test-ap-events"))
               (:file "test-ap-delivery" :depends-on ("package" "test-crypto" "test-ap-inbox")))
  :perform (test-op (op c)
             (uiop:symbol-call :fiveam :run!
                               (uiop:find-symbol* :all-tests :site.tests))))
