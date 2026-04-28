;;; ActivityPub replies / reactions: functions that read events from
;;; storage and decode ActivityStreams Create payloads into blog-renderable
;;; plists. All storage access is through site.storage, so these are
;;; unit-testable against the memory backend.

(in-package :site.activitypub)

(defun dislike (likeid)
  "Delete the Like or Announce event identified by LIKEID."
  (site.storage:event-delete likeid))

(defun reactions-number (id reaction-type)
  "Count reactions of REACTION-TYPE (\"Like\" or \"Announce\") for post ID."
  (site.storage:event-count-by-object-and-type
   (format nil "https://rayslava.com/blog?id=~A" id)
   reaction-type))

(defun direct-replies (id)
  "Get comments for ID that arrived as Create replies. site.storage returns
plists with :event holding the raw JSON string; we decode each and pull
out the ActivityStreams fields the blog renderer consumes."
  (let ((result nil)
        (replies (site.storage:event-scan-by-reply-and-type
                  (format nil "https://rayslava.com/blog?id=~A" id)
                  "Create")))
    (dolist (reply-row replies result)
      (let*  ((reply (cl-json:decode-json-from-string (getf reply-row :event)))
	      (object (cdr (assoc :object reply)))
	      (actor (cdr (assoc :attributed-to object)))
	      (url (cdr (assoc :url object)))
	      (published (local-time:timestamp-to-universal
                          (local-time:parse-timestring
                           (cdr (assoc :published object)))))
	      (content (cdr (assoc :content object)))
	      (to (cdr (assoc :to object)))
	      (cc (cdr (assoc :cc object)))
	      (public (if (or (member "https://www.w3.org/ns/activitystreams#Public" to :test #'string-equal)
			      (member "https://www.w3.org/ns/activitystreams#Public" cc :test #'string-equal))
			  t
			  nil)))
	(push (pairlis '(:id :actor :url :published :content :public)
		       (list id actor url published content public))
	      result)))))

(defun get-all-replies (post-id)
  "Retrieve the full list of replies for POST-ID, recursively descending
into nested replies. Note: the recursion uses getf on the alists produced
by direct-replies, so today it only returns one level unless the caller
later normalizes the shape. Left as-is to preserve existing behavior."
  (let ((replies (direct-replies post-id))
        (all-replies '()))
    (dolist (reply replies)
      (push reply all-replies)
      (when (getf reply :id)
        (setf (getf reply :replies) (get-all-replies (getf reply :id)))
        (dolist (nested-reply (getf reply :replies))
          (push nested-reply all-replies))))
    (nreverse all-replies)))

(defun flatten-replies (replies)
  "Flatten the nested list of replies into a flat list."
  (let ((flat-replies '()))
    (dolist (reply replies)
      (push reply flat-replies)
      (when (getf reply :replies)
        (setf (getf reply :replies) (flatten-replies (getf reply :replies)))
        (dolist (nested-reply (getf reply :replies))
          (push nested-reply flat-replies))))
    (nreverse flat-replies)))
