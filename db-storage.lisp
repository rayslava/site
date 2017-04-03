(defpackage :site.db-storage
  (:use :cl :site.config :dyna.table-operation))

(in-package :site.db-storage)

;;; Static storage procedure
(defclass static-file ()
  ((filename :key-type :hash
	     :attr-type :S
	     :initarg :forum-name
	     :accessor file-filename)
   (location :key-type :range
	     :attr-type :S
	     :initarg :subject
	     :accessor file-location))
  (:dyna *dyna*)
  (:metaclass <dyna-table-class>))
