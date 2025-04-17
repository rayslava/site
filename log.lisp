;;; Logging management
(defpackage :site.log
  (:use :cl :bordeaux-threads :trivial-gray-streams :asdf :local-time
   :hunchentoot :cl-who :site.db-manage :site.config)
  (:export
   :*log-info-stream*
   :*log-error-stream*
   :*log-access-stream*))

(in-package :site.log)

;; Create a circular buffer for storing log messages
(defvar *log-messages* (make-array 1000 :initial-element nil :fill-pointer 0)
  "Circular buffer to store the most recent log messages")

(defvar *log-messages-lock* (bt:make-lock "log-messages-lock")
  "Lock for thread-safe access to the log messages buffer")

(defvar *log-message-index* 0
  "Current index in the circular buffer")

(defun add-log-message (level message)
  "Add `MESSAGE' with `LEVEL' (:info, :error, etc.) to the circular buffer and
write to appropriate stream."
  (bt:with-lock-held (*log-messages-lock*)
    (let* ((timestamp (local-time:format-timestring
		       nil (local-time:now)
		       :format '((:year 4) "-" (:month 2) "-" (:day 2) " "
				 (:hour 2) ":" (:min 2) ":" (:sec 2))))
           (formatted-message (format nil "[~A] [~A] ~A" timestamp (string-upcase (string level)) message)))
      ;; Buffer
      (setf (aref *log-messages* *log-message-index*) formatted-message)
      (when (< (fill-pointer *log-messages*) 1000)
        (vector-push formatted-message *log-messages*))
      (setf *log-message-index* (mod (1+ *log-message-index*) 1000))

      ;; Route to appropriate stream
      (cond
        ((or (eq level :info) (eq level :access))
         (format *access-log-file* "~A~%" formatted-message))
        ((eq level :error)
         (format *message-log-file* "~A~%" formatted-message))
        (t
         (format *message-log-file* "[UNKNOWN LEVEL ~A] ~A~%" level formatted-message))))))

(defclass log-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((level :initarg :level :reader log-stream-level)
   (buffer :initform (make-string-output-stream) :accessor log-stream-buffer)))

(defmethod stream-write-char ((stream log-stream) char)
  (write-char char (log-stream-buffer stream))
  (when (char= char #\Newline)
    ;; Flush buffer into log
    (let ((string (get-output-stream-string (log-stream-buffer stream))))
      ;; Remove the newline from the end if needed
      (setf string (string-trim '(#\Newline #\Return) string))
      (add-log-message (log-stream-level stream) string)
      ;; Reset buffer
      (setf (slot-value stream 'buffer) (make-string-output-stream))))
  char)

(defmethod stream-write-string ((stream log-stream) string &optional start end)
  (loop for i from (or start 0) below (or end (length string))
        do (stream-write-char stream (char string i)))
  string)

(defmethod stream-terpri ((stream log-stream))
  (stream-write-char stream #\Newline)
  nil)

(defmethod stream-finish-output ((stream log-stream)) nil)

(defparameter *log-info-stream* (make-instance 'log-stream :level :info))
(defparameter *log-error-stream* (make-instance 'log-stream :level :error))
(defparameter *log-access-stream* (make-instance 'log-stream :level :access))

;; Handler function for the logs page
(define-easy-handler (logs :uri "/admin/logs"
			   :default-request-type :get)
    ()
  (with-http-authentication
      (no-cache)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (:head
        (:title "Server Logs")
	(:link :rel "stylesheet" :type "text/css" :href "/main.css")
	(:link :rel "stylesheet" :type "text/css" :href "/log.css")
	(:body
	 (:h1 "Server Logs")
	 (:button :class "refresh-btn" :onclick "location.reload();" "Refresh Logs")
	 (:div :class "log-container"
	       (bt:with-lock-held (*log-messages-lock*)
		 (let ((logs (loop for i from 0 below (fill-pointer *log-messages*)
				   for msg = (aref *log-messages* i)
				   when msg collect msg)))
		   (dolist (log (reverse logs))
		     (let ((class (cond
				    ((search "[ERROR]" log) "error")
				    ((search "[INFO]" log) "info")
				    ((search "[ACCESS]" log) "access")
				    (t ""))))
		       (htm (:div :class (format nil "log-entry ~A" class) (str log))))))))))))))
