;;; Storage abstraction.
;;;
;;; All DynamoDB and S3 access funnels through generic functions dispatched
;;; on a backend object bound to SITE.STORAGE:*BACKEND*. Three concrete
;;; backends:
;;;
;;;  DYNA-BACKEND    - production path; methods defined in storage-dyna.lisp
;;;                    so this file doesn't forward-reference activitypub/
;;;                    db-storage types.
;;;  MEMORY-BACKEND  - pure in-process hash tables, for tests.
;;;  LOCAL-BACKEND   - subscribers/events in memory, static blobs on disk.
;;;
;;; Abstract only the exact call sites in activitypub.lisp and db-storage.lisp.
;;; When a new need arises, add a generic function here and a method per
;;; backend — don't grow a generic wrapper around every dyna feature.

(defpackage :site.storage
  (:use :cl)
  (:export
   :*backend*
   :detect-backend
   :make-backend
   :backend
   :dyna-backend
   :memory-backend
   :local-backend
   :local-static-dir
   :mem-subscribers
   :mem-events
   :mem-statics
   :mem-blobs
   ;; Subscriber ops
   :subscriber-save
   :subscriber-find
   :subscriber-delete
   :subscriber-update-lastpost
   :subscriber-all-with-lastpost<
   :subscriber-all-with-lastpost>=
   ;; Event ops
   :event-save
   :event-find-by-id
   :event-delete
   :event-count-by-object-and-type
   :event-scan-by-reply-and-type
   ;; Static storage ops
   :static-list
   :static-find-by-s3name
   :static-save
   :static-delete
   :static-public-url
   :static-blob-put
   ;; Table bootstrapping
   :ensure-subscriber-table
   :ensure-event-table
   :ensure-static-table
   :ensure-bucket
   ;; Low-level backend generics exported so the dyna backend can specialize
   :%subscriber-save :%subscriber-find :%subscriber-delete
   :%subscriber-update-lastpost
   :%subscriber-all-with-lastpost< :%subscriber-all-with-lastpost>=
   :%event-save :%event-find-by-id :%event-delete
   :%event-count-by-object-and-type :%event-scan-by-reply-and-type
   :%static-list :%static-find-by-s3name :%static-save :%static-delete
   :%static-public-url :%static-blob-put
   :%ensure-subscriber-table :%ensure-event-table :%ensure-static-table
   :%ensure-bucket))

(in-package :site.storage)

(defclass backend () ()
  (:documentation "Abstract base class for storage backends."))

(defclass dyna-backend (backend) ()
  (:documentation "Production backend: DynamoDB + S3. Methods live in
storage-dyna.lisp so this file stays free of forward references."))

(defclass memory-backend (backend)
  ((subscribers :initform (make-hash-table :test 'equal) :reader mem-subscribers)
   (events      :initform (make-hash-table :test 'equal) :reader mem-events)
   (statics     :initform (make-hash-table :test 'equal) :reader mem-statics)
   (blobs       :initform (make-hash-table :test 'equal) :reader mem-blobs))
  (:documentation "Test backend: everything in hash tables."))

(defclass local-backend (memory-backend)
  ((static-dir :initarg :static-dir :reader local-static-dir))
  (:documentation "Local dev backend: memory for structured data, local
filesystem for static blobs."))

(defvar *backend* nil
  "The active storage backend. Bound by start-server and by tests.")

(defun detect-backend ()
  "Choose a backend class from the environment."
  (let ((k (uiop:getenv "SITE_STORAGE")))
    (cond
      ((and k (string-equal k "memory")) :memory)
      ((and k (string-equal k "local"))  :local)
      ((and k (string-equal k "dyna"))   :dyna)
      ((and (uiop:getenv "AWS_ACCESS_KEY")
            (not (string= "" (uiop:getenv "AWS_ACCESS_KEY"))))
       :dyna)
      (t :local))))

(defun make-backend (kind &key static-dir)
  "Instantiate a backend by keyword: :DYNA, :MEMORY, :LOCAL."
  (ecase kind
    (:dyna   (make-instance 'dyna-backend))
    (:memory (make-instance 'memory-backend))
    (:local  (make-instance 'local-backend
                            :static-dir (or static-dir
                                            (merge-pathnames
                                             "uploads/"
                                             (uiop:getcwd)))))))

;;;; ---------------------------------------------------------------------
;;;; Generics.

(defgeneric %subscriber-save (backend actor attr))
(defgeneric %subscriber-find (backend actor))
(defgeneric %subscriber-delete (backend actor))
(defgeneric %subscriber-update-lastpost (backend actor new-lastpost))
(defgeneric %subscriber-all-with-lastpost< (backend limit))
(defgeneric %subscriber-all-with-lastpost>= (backend limit))

(defgeneric %event-save (backend event))
(defgeneric %event-find-by-id (backend id))
(defgeneric %event-delete (backend id))
(defgeneric %event-count-by-object-and-type (backend object-id event-type))
(defgeneric %event-scan-by-reply-and-type (backend reply-to event-type))

(defgeneric %static-list (backend))
(defgeneric %static-find-by-s3name (backend s3name))
(defgeneric %static-save (backend entry))
(defgeneric %static-delete (backend s3name))
(defgeneric %static-public-url (backend s3name))
(defgeneric %static-blob-put (backend s3name local-path content-type))

(defgeneric %ensure-subscriber-table (backend))
(defgeneric %ensure-event-table (backend))
(defgeneric %ensure-static-table (backend))
(defgeneric %ensure-bucket (backend))

;;;; ---------------------------------------------------------------------
;;;; Thin wrappers. Callers use these; never reach for methods directly.

(defun subscriber-save (actor attr)
  "Insert or overwrite a subscriber row. ATTR is an alist stored verbatim."
  (%subscriber-save *backend* actor attr))
(defun subscriber-find (actor)
  "Return a plist of slot values for ACTOR, or NIL."
  (%subscriber-find *backend* actor))
(defun subscriber-delete (actor)
  (%subscriber-delete *backend* actor))
(defun subscriber-update-lastpost (actor new-lastpost)
  (%subscriber-update-lastpost *backend* actor new-lastpost))
(defun subscriber-all-with-lastpost< (limit)
  "All subscribers whose lastpost is strictly less than LIMIT."
  (%subscriber-all-with-lastpost< *backend* limit))
(defun subscriber-all-with-lastpost>= (limit)
  (%subscriber-all-with-lastpost>= *backend* limit))

(defun event-save (event)
  "EVENT is a plist: :id :object-id :published :event-type :event :reply-to."
  (%event-save *backend* event))
(defun event-find-by-id (id)
  (%event-find-by-id *backend* id))
(defun event-delete (id)
  (%event-delete *backend* id))
(defun event-count-by-object-and-type (object-id event-type)
  (%event-count-by-object-and-type *backend* object-id event-type))
(defun event-scan-by-reply-and-type (reply-to event-type)
  (%event-scan-by-reply-and-type *backend* reply-to event-type))

(defun static-list ()
  (%static-list *backend*))
(defun static-find-by-s3name (s3name)
  (%static-find-by-s3name *backend* s3name))
(defun static-save (entry)
  "ENTRY is a plist: :s3name :filename :attr."
  (%static-save *backend* entry))
(defun static-delete (s3name)
  (%static-delete *backend* s3name))
(defun static-public-url (s3name)
  "Return a URL at which S3NAME can be fetched publicly."
  (%static-public-url *backend* s3name))
(defun static-blob-put (s3name local-path content-type)
  "Upload the bytes of LOCAL-PATH under the storage key S3NAME."
  (%static-blob-put *backend* s3name local-path content-type))

(defun ensure-subscriber-table ()
  (%ensure-subscriber-table *backend*))
(defun ensure-event-table ()
  (%ensure-event-table *backend*))
(defun ensure-static-table ()
  (%ensure-static-table *backend*))
(defun ensure-bucket ()
  (%ensure-bucket *backend*))

;;;; ---------------------------------------------------------------------
;;;; MEMORY-BACKEND methods.

(defmethod %subscriber-save ((b memory-backend) actor attr)
  (let ((existing (gethash actor (mem-subscribers b))))
    (setf (gethash actor (mem-subscribers b))
          (list :actor actor
                :lastpost (or (and existing (getf existing :lastpost)) 0)
                :attr attr)))
  t)

(defmethod %subscriber-find ((b memory-backend) actor)
  (gethash actor (mem-subscribers b)))

(defmethod %subscriber-delete ((b memory-backend) actor)
  (remhash actor (mem-subscribers b)))

(defmethod %subscriber-update-lastpost ((b memory-backend) actor new-lastpost)
  (let ((row (gethash actor (mem-subscribers b))))
    (when row
      (setf (getf row :lastpost) new-lastpost)
      (setf (gethash actor (mem-subscribers b)) row))))

(defmethod %subscriber-all-with-lastpost< ((b memory-backend) limit)
  (loop for row being the hash-values of (mem-subscribers b)
        when (< (getf row :lastpost) limit)
          collect row))

(defmethod %subscriber-all-with-lastpost>= ((b memory-backend) limit)
  (loop for row being the hash-values of (mem-subscribers b)
        when (>= (getf row :lastpost) limit)
          collect row))

(defmethod %event-save ((b memory-backend) event)
  (setf (gethash (getf event :id) (mem-events b)) event)
  t)

(defmethod %event-find-by-id ((b memory-backend) id)
  (gethash id (mem-events b)))

(defmethod %event-delete ((b memory-backend) id)
  (remhash id (mem-events b)))

(defmethod %event-count-by-object-and-type ((b memory-backend) object-id event-type)
  (loop for ev being the hash-values of (mem-events b)
        count (and (equal (getf ev :object-id) object-id)
                   (equal (getf ev :event-type) event-type))))

(defmethod %event-scan-by-reply-and-type ((b memory-backend) reply-to event-type)
  (loop for ev being the hash-values of (mem-events b)
        when (and (equal (getf ev :reply-to) reply-to)
                  (equal (getf ev :event-type) event-type))
          collect ev))

(defmethod %static-list ((b memory-backend))
  (loop for row being the hash-values of (mem-statics b) collect row))

(defmethod %static-find-by-s3name ((b memory-backend) s3name)
  (gethash s3name (mem-statics b)))

(defmethod %static-save ((b memory-backend) entry)
  (setf (gethash (getf entry :s3name) (mem-statics b)) entry)
  t)

(defmethod %static-delete ((b memory-backend) s3name)
  (remhash s3name (mem-statics b))
  (remhash s3name (mem-blobs b)))

(defmethod %static-public-url ((b memory-backend) s3name)
  (format nil "memory://~A" s3name))

(defmethod %static-blob-put ((b memory-backend) s3name local-path content-type)
  (setf (gethash s3name (mem-blobs b))
        (list :local-path local-path :content-type content-type))
  t)

(defmethod %ensure-subscriber-table ((b memory-backend)) t)
(defmethod %ensure-event-table      ((b memory-backend)) t)
(defmethod %ensure-static-table     ((b memory-backend)) t)
(defmethod %ensure-bucket           ((b memory-backend)) t)

;;;; ---------------------------------------------------------------------
;;;; LOCAL-BACKEND methods — reuse memory for structured data, override
;;;; blob placement to write to disk.

(defmethod %static-blob-put ((b local-backend) s3name local-path content-type)
  (declare (ignore content-type))
  (ensure-directories-exist (local-static-dir b))
  (let ((target (merge-pathnames s3name (local-static-dir b))))
    (uiop:copy-file local-path target)
    t))

(defmethod %static-public-url ((b local-backend) s3name)
  (concatenate 'string "/uploads/" s3name))

(defmethod %ensure-static-table ((b local-backend))
  (ensure-directories-exist (local-static-dir b))
  t)
