(asdf:oos 'asdf:load-op :hunchentoot)
(asdf:oos 'asdf:load-op :cl-who)
(asdf:oos 'asdf:load-op :ht-simple-ajax)

(defpackage #:piserv-asd
  (:use :cl :asdf))

(in-package :piserv-asd)

(defsystem piserv
  :name "piserv"
  :version "0.1"
  :maintainer "rayslava"
  :author "rayslava"
  :licence "BSD"
  :description "The homepage server"
  :long-description "Lisp implementation of my home page"
  :components ((:file "site"
                      :depends-on ("static"))
               (:file "static")))
