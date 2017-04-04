(defpackage :site.db-storage
  (:use :cl :site.config :dyna.table-operation :zs3 :jonathan :trivial-mimes)
  (:export
   :list-available-statics
   :static-file
   :filename
   :attr))

(in-package :site.db-storage)

;;; Static storage procedure
(defclass static-file ()
  ((filename :key-type :hash
	     :attr-name "Filename"
	     :attr-type :S
	     :initarg :filename
	     :accessor filename
	     :documentation "The initial file name. Unique and is primary key.")
   (attr     :attr-name "Attr"
	     :attr-type :S
	     :initarg :attr
	     :key-type :range
	     :accessor attr
	     :initform nil
	     :documentation "Sorted plist with additional arguments"))
  (:dyna *dyna*)
  (:table-name "statics")
  (:metaclass dyna.table-operation::<dyna-table-class>))

(defmethod print-object ((file static-file) out)
  (with-slots (filename attr) file
    (print-unreadable-object (file out :type t)
      (format out "File '~A' ~A" filename attr))))

(defmethod store-ref-to-dynamodb (file static-file)
  "Store the static file reference to DynamoDB taking data from `file' object"
  (save-dyna file))

(defun upload-file (file &optional attr)
  "Upload `file' into S3 and create `static-file' record for it in DynamoDB"
  (let* ((key (concatenate 'string
			   (pathname-name file) "." (pathname-type file)))
	 (response (put-file file *static-bucket* key
			     :public t :content-type (mime file))))
    (if (not (eq (http-code response) 200))
	(format nil "Couldn't upload :(")
	(let ((obj (make-instance 'static-file :filename key)))
	  (when attr
	    (setf (attr obj)
		  (to-json attr)))
	  (save-dyna obj)))))

(defun list-available-statics ()
  "Request list of files from DynamoDB and return as list"
  (select-dyna 'static-file))

;;; We have to create the bucket if there is no one
(when (not (bucket-exists-p *static-bucket*))
  (create-bucket *static-bucket* :location *aws-region*))

;;; The same to DynamoDB table
(when (not (table-exist-p 'static-file))
  (create-dyna-table 'static-file))
