(defpackage :site.db-storage
  (:use :cl :site.config :dyna.table-operation :zs3))

(in-package :site.db-storage)

;;; Static storage procedure
(defclass static-file ()
  ((filename :key-type :hash
	     :attr-type :S
	     :initarg :filename
	     :accessor file-filename
	     :documentation "The initial file name. Unique usually")
   (attr     :attr-type :S
	     :initarg :attr
	     :accessor attr
	     :initform nil
	     :documentation "Sorted plist with additional arguments"))
   (:dyna *dyna*)
   (:metaclass dyna.table-operation::<dyna-table-class>))

(defmethod print-object ((file static-file) out)
  (with-slots (filename attr) file
    (print-unreadable-object (file out :type t)
      (format out "File '~A' ~A" filename attr))))

(defmethod store-ref-to-dynamodb (file static-file)
  "Stores the static file reference to DynamoDB taking data from `file' object"
  (save-dyna file))

(defun upload-file (file)
  "Creates `static-file' record for `file' argument"
  (let* ((key (concatenate 'string
			  (pathname-name file) "." (pathname-type file)))
	(response (put-file file *static-bucket* key)))
    (when (not (eq (http-code response) 200))
      (format nil "Couldn't upload :("))
    (make-instance 'static-file
		   :filename key)))

;;; We have to create the bucket if there is no one
(when (not (bucket-exists-p *static-bucket*))
  (create-bucket *static-bucket* :location *aws-region*))

(when (eq (distributions-for-bucket *static-bucket*) nil)
  (progn
    (setf *distribution* (create-distribution *static-bucket*))
    (when (not (string= (status *distribution*) "Deployed"))
      (progn
	(sleep 60)
	(refresh *distribution*)))))
