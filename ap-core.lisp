;;; ActivityPub core: the shared package definition, the two dyna
;;; classes, their print-object methods, and the table-provisioning
;;; helper. Everything else in the site.activitypub package lives in
;;; sibling ap-*.lisp files that `(in-package :site.activitypub)`.

(defpackage :site.activitypub
  (:use :cl :hunchentoot :cl-who :cl-json
	:asdf :dyna.table-operation :dyna
	:site.db-manage :site.config :site.crypto :site.blog-post
        :site.blog-registry :site.ap-signature)
  (:shadowing-import-from :cl-json-helper :json-bool)
  (:export :maybe-deliver-new-posts
           :maybe-update-post
           :reactions-number
           :direct-replies
           :get-all-replies
           :flatten-replies
           :fedi-note-create
           :fedi-post-create
           :fedi-post-update
           :prepare-fedi-object
           :prepare-fedi-note
           :prepare-image-attachments
           :generate-outbox-collection
           :generate-accept
           :generate-signed-header
           :send-signed
           :*http-send-fn*
           :handle-inbox-activity
           :ensure-activitypub-storage!
           :dislike))

(in-package :site.activitypub)

(defclass activitypub-subscriber ()
  ((actor :key-type :hash
	  :attr-name "actor"
	  :attr-type :S
	  :initarg :actor
	  :accessor actor
	  :documentation "The actor url of subscriber.")
   (lastpost :key-type :range
	     :attr-name "lastpost"
	     :attr-type :N
	     :initarg :lastpost
	     :accessor lastpost
	     :initform 0
	     :documentation "Id of last post sent to this subscriber")
   (attr :key-type :range
	 :attr-name "attr"
	 :attr-type :S
	 :initarg :attr
	 :accessor attr
	 :initform nil
	 :documentation "Sorted plist with additional arguments"))
  (:dyna *dyna*)
  (:table-name "activitypub-subscribers")
  (:metaclass dyna.table-operation::<dyna-table-class>))

(defmethod print-object ((subscriber activitypub-subscriber) out)
  (with-slots (actor lastpost attr) subscriber
    (print-unreadable-object (subscriber out :type t)
      (format out "File '~A' with last post provided ~A ~A" actor lastpost attr))))

(defclass activitypub-event ()
  ((id :key-type :hash
       :attr-name "id"
       :attr-type :S
       :initarg :id
       :accessor id
       :documentation "ID of the event")
   (published :key-type :range
	      :attr-name "published"
	      :attr-type :N
	      :initarg :published
	      :accessor published
	      :initform 0
	      :documentation "Timestamp of event from published field")
   (event-type :key-type :range
	       :attr-name "eventtype"
	       :attr-type :S
	       :initarg :event-type
	       :accessor event-type
	       :initform nil
	       :documentation "Type of the event")
   (object-id :key-type :hash
	      :attr-name "objectid"
	      :attr-type :S
	      :initarg :object-id
	      :accessor object-id
	      :documentation "ID of the object event is related to")
   (reply-to  :key-type :hash
	      :attr-name "replyto"
	      :attr-type :S
	      :initarg :reply-to
	      :accessor reply-to
	      :initform nil
	      :documentation "ID of the object to reply, if exists")
   (event :key-type :range
	  :attr-name "event"
	  :attr-type :S
	  :initarg :event
	  :accessor event
	  :initform nil
	  :documentation "Full event body"))
  (:dyna *dyna*)
  (:table-name "activitypub-events")
  (:metaclass dyna.table-operation::<dyna-table-class>))

(defmethod print-object ((event activitypub-event) out)
  (with-slots (id published event-type) event
    (print-unreadable-object (event out :type t)
      (format out "ActivityPub event '~A' of type ~A published at ~A" id event-type published))))

(defun ensure-activitypub-storage! ()
  "Create DynamoDB tables for ActivityPub subscribers and events if missing.
Must be called after configure-aws! + initialize-dyna!. Safe to call
repeatedly. The memory and local backends no-op both ensure-* calls."
  (site.storage:ensure-subscriber-table)
  (site.storage:ensure-event-table))
