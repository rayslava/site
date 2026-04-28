(defpackage :site.db-storage
  (:use :cl :site.config :dyna.table-operation :dyna :zs3 :jonathan :trivial-mimes)
  (:export
   :list-available-statics
   :static-file
   :filename
   :attr
   :s3name
   :upload-file
   :compute-s3-name
   :delete-static
   :ensure-static-storage!))

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

(defun compute-s3-name (name)
  "Generate the md5 hash of `name' for S3 storage"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5 (ironclad:ascii-string-to-byte-array name))))

(defun upload-file (file &optional attr)
  "Upload FILE and register a static-file record pointing at it.
Blob and metadata both flow through site.storage so the dev/local
backend can keep everything in-tree without touching AWS."
  (let* ((name (getf attr :filename))
         (key (compute-s3-name name)))
    (if (not (site.storage:static-blob-put key file (mime file)))
        (format nil "Couldn't upload :(")
        (site.storage:static-save
         (list :filename name
               :s3name key
               :attr (when attr (to-json attr)))))))

(defun list-available-statics ()
  "Return all registered static files as plists (:s3name :filename :attr)."
  (site.storage:static-list))

(defun delete-static (s3name)
  "Delete the object with S3NAME from both metadata store and blob store."
  (site.storage:static-delete s3name))

(defun ensure-static-storage! ()
  "Provision the metadata and blob stores for whichever backend is active.
Safe to call repeatedly."
  (site.storage:ensure-bucket)
  (site.storage:ensure-static-table))
