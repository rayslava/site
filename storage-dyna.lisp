;;; DYNA-BACKEND methods for site.storage.
;;;
;;; Lives in its own file so storage.lisp doesn't forward-reference the
;;; activitypub and db-storage class symbols. Loads after those files in
;;; site.asd. Each method wraps the exact dyna / zs3 sequence that used
;;; to live in activitypub.lisp and db-storage.lisp — the refactor is
;;; behavior-preserving, not a schema change.

(in-package :site.storage)

;;;; ---------------------------------------------------------------------
;;;; Subscribers

(defmethod %subscriber-save ((b dyna-backend) actor attr)
  (let ((row (make-instance 'site.activitypub::activitypub-subscriber
                            :actor actor
                            :attr (cl-json:encode-json-to-string attr))))
    (dyna:save-dyna row)))

(defun %subscriber-row->plist (item)
  (list :actor (site.activitypub::actor item)
        :lastpost (site.activitypub::lastpost item)
        :attr (site.activitypub::attr item)
        :row item))

(defmethod %subscriber-find ((b dyna-backend) actor)
  (let ((item (car (dyna:select-dyna 'site.activitypub::activitypub-subscriber
                                     (sxql:where (:= :actor actor))))))
    (when item (%subscriber-row->plist item))))

(defmethod %subscriber-delete ((b dyna-backend) actor)
  (let ((item (car (dyna:select-dyna 'site.activitypub::activitypub-subscriber
                                     (sxql:where (:= :actor actor))))))
    (when item
      (dyna::delete-item site.config:*dyna*
                         :table-name "activitypub-subscribers"
                         :key `(("actor" . ,(site.activitypub::actor item))
                                ("lastpost" . ,(site.activitypub::lastpost item)))))))

(defmethod %subscriber-update-lastpost ((b dyna-backend) actor new-lastpost)
  (let* ((item (car (dyna:select-dyna 'site.activitypub::activitypub-subscriber
                                      (sxql:where (:= :actor actor)))))
         (oldposts (and item (site.activitypub::lastpost item))))
    (when item
      (setf (site.activitypub::lastpost item) new-lastpost)
      (dyna::delete-item site.config:*dyna*
                         :table-name "activitypub-subscribers"
                         :key `(("actor" . ,(site.activitypub::actor item))
                                ("lastpost" . ,oldposts)))
      (dyna:save-dyna item))))

(defmethod %subscriber-all-with-lastpost< ((b dyna-backend) limit)
  (mapcar #'%subscriber-row->plist
          (dyna:select-dyna 'site.activitypub::activitypub-subscriber
                            (sxql:where (:< :lastpost limit)))))

(defmethod %subscriber-all-with-lastpost>= ((b dyna-backend) limit)
  (mapcar #'%subscriber-row->plist
          (dyna:select-dyna 'site.activitypub::activitypub-subscriber
                            (sxql:where (:>= :lastpost limit)))))

;;;; ---------------------------------------------------------------------
;;;; Events

(defmethod %event-save ((b dyna-backend) event)
  (let ((row (make-instance 'site.activitypub::activitypub-event
                            :id (getf event :id)
                            :object-id (getf event :object-id)
                            :published (or (getf event :published) 0)
                            :event-type (getf event :event-type)
                            :event (getf event :event)
                            :reply-to (getf event :reply-to))))
    (dyna:save-dyna row)))

(defmethod %event-find-by-id ((b dyna-backend) id)
  (let ((item (car (dyna:select-dyna 'site.activitypub::activitypub-event
                                     (sxql:where (:= :id id))))))
    (when item
      (list :id (site.activitypub::id item)
            :object-id (site.activitypub::object-id item)
            :published (site.activitypub::published item)
            :event-type (site.activitypub::event-type item)
            :event (site.activitypub::event item)
            :reply-to (site.activitypub::reply-to item)
            :row item))))

(defmethod %event-delete ((b dyna-backend) id)
  (let ((item (car (dyna:select-dyna 'site.activitypub::activitypub-event
                                     (sxql:where (:= :id id))))))
    (when item
      (dyna::delete-item site.config:*dyna*
                         :table-name "activitypub-events"
                         :key `(("id" . ,(site.activitypub::id item))
                                ("published" . 0))))))

(defmethod %event-count-by-object-and-type ((b dyna-backend) object-id event-type)
  (handler-case
      (let ((counts
              (nth-value 1 (dyna::scan site.config:*dyna*
                                       :table-name "activitypub-events"
                                       :filter-expression "objectid = :id AND eventtype = :type"
                                       :expression-attribute-values
                                       `((":id" . ,object-id)
                                         (":type" . ,event-type))
                                       :select "COUNT"))))
        (cdr (assoc "count" (cdr counts) :test #'string-equal)))
    (error () -1)))

(defmethod %event-scan-by-reply-and-type ((b dyna-backend) reply-to event-type)
  "Return plists with :event holding the raw JSON string, matching the
memory backend's shape. direct-replies then parses :event uniformly."
  (let* ((response
           (nth-value 1 (dyna::scan site.config:*dyna*
                                    :table-name "activitypub-events"
                                    :filter-expression "replyto = :id AND eventtype = :type"
                                    :expression-attribute-values
                                    `((":id" . ,reply-to)
                                      (":type" . ,event-type)))))
         (items (cdr (assoc "items" (cdr response) :test #'string-equal))))
    (mapcar (lambda (raw)
              (list :event
                    (cdr (assoc "s" (cddr (assoc "event" (cdr raw) :test #'string-equal))
                                :test #'string-equal))))
            items)))

;;;; ---------------------------------------------------------------------
;;;; Static files

(defmethod %static-list ((b dyna-backend))
  (mapcar (lambda (item)
            (list :s3name (site.db-storage:s3name item)
                  :filename (site.db-storage:filename item)
                  :attr (site.db-storage:attr item)
                  :row item))
          (dyna:select-dyna 'site.db-storage:static-file)))

(defmethod %static-find-by-s3name ((b dyna-backend) s3name)
  (let ((item (car (dyna:select-dyna 'site.db-storage:static-file
                                     (sxql:where (:= :s3name s3name))))))
    (when item
      (list :s3name (site.db-storage:s3name item)
            :filename (site.db-storage:filename item)
            :attr (site.db-storage:attr item)
            :row item))))

(defmethod %static-save ((b dyna-backend) entry)
  (let ((row (make-instance 'site.db-storage:static-file
                            :filename (getf entry :filename)
                            :s3name (getf entry :s3name))))
    (when (getf entry :attr)
      (setf (site.db-storage:attr row) (getf entry :attr)))
    (dyna:save-dyna row)))

(defmethod %static-delete ((b dyna-backend) s3name)
  (let ((item (car (dyna:select-dyna 'site.db-storage:static-file
                                     (sxql:where (:= :s3name s3name))))))
    (when item
      (dyna::delete-item site.config:*dyna*
                         :table-name "statics"
                         :key `(("s3name" . ,s3name)
                                ("filename" . ,(site.db-storage:filename item))))
      (zs3:delete-object site.config:*static-bucket* s3name))))

(defmethod %static-public-url ((b dyna-backend) s3name)
  (concatenate 'string "https://" zs3:*s3-endpoint* "/"
               site.config:*static-bucket* "/" s3name))

(defmethod %static-blob-put ((b dyna-backend) s3name local-path content-type)
  (let ((response (zs3:put-file local-path site.config:*static-bucket* s3name
                                :public t :content-type content-type)))
    (= 200 (zs3:http-code response))))

;;;; ---------------------------------------------------------------------
;;;; Table / bucket bootstrapping

(defmethod %ensure-subscriber-table ((b dyna-backend))
  (unless (dyna.table-operation:table-exist-p 'site.activitypub::activitypub-subscriber)
    (dyna.table-operation:create-dyna-table 'site.activitypub::activitypub-subscriber)))

(defmethod %ensure-event-table ((b dyna-backend))
  (unless (dyna.table-operation:table-exist-p 'site.activitypub::activitypub-event)
    (dyna.table-operation:create-dyna-table 'site.activitypub::activitypub-event)))

(defmethod %ensure-static-table ((b dyna-backend))
  (unless (dyna.table-operation:table-exist-p 'site.db-storage:static-file)
    (dyna.table-operation:create-dyna-table 'site.db-storage:static-file)))

(defmethod %ensure-bucket ((b dyna-backend))
  (unless (zs3:bucket-exists-p site.config:*static-bucket*)
    (zs3:create-bucket site.config:*static-bucket* :location site.config:*aws-region*)))
