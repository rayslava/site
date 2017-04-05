(defpackage :site.db-storage
  (:use :cl :site.config :dyna.table-operation :zs3 :jonathan :trivial-mimes)
  (:export
   :list-available-statics
   :static-file
   :filename
   :attr
   :s3name
   :upload-file
   :compute-s3-name))

(in-package :site.db-storage)

;;; Static storage procedure
(defclass static-file ()
  ((s3name   :key-type :hash
	     :attr-name "s3name"
	     :attr-type :S
	     :initarg :s3name
	     :accessor s3name
	     :documentation "The name in s3 storage.")
   (filename :key-type :range
	     :attr-name "filename"
	     :attr-type :S
	     :initarg :filename
	     :accessor filename
	     :documentation "The initial file name. Unique and is primary key.")
   (attr     :attr-name "attr"
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

(defun compute-s3-name (name)
  "Generate the md5 hash of `name' for S3 storage"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5 (ironclad:ascii-string-to-byte-array name))))

(defun upload-file (file &optional attr)
  "Upload `file' into S3 and create `static-file' record for it in DynamoDB"
  (let* ((name (getf attr :filename))
	 (key (compute-s3-name name))
	 (response (put-file file *static-bucket* key
			     :public t :content-type (mime file))))
    (if (not (eq (http-code response) 200))
	(format nil "Couldn't upload :(")
	(let ((obj (make-instance 'static-file :filename name :s3name key)))
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
