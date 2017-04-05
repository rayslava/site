(defpackage :site.config
  (:use :cl :asdf :zs3 :dyna)
  (:export :*admin-login*
	   :*admin-password*
	   :*access-log-file*
	   :*message-log-file*
	   :*default-static-path*
	   :*server-port*
	   :*admin-login-message*
	   :environment-credentials
	   :*credentials*
	   :*dyna*
	   :*static-bucket*
	   :*aws-region*
	   :*distribution*))

(in-package :site.config)

(defvar *admin-login* "admin"
  "Admin page password")

(defvar *admin-password*
  (let ((envpass (asdf::getenv "ADMIN_PASSWORD")))
    (if envpass
	envpass
	"adminpassword"))
  "Admin page password")

(defvar *access-log-file* "access.log"
  "Server access log file")

(defvar *message-log-file* "message.log"
  "Server message log file")

(defvar *server-port* 8080
  "Default server port")

(defvar *admin-login-message* "Please enter admin credentials"
  "Message which will appear if user tries to get access to admin page")

(defvar *default-static-path* (concatenate 'string (namestring  (sb-posix::getcwd)) "/" "static")
  "Default path where server should search for files that should be exported as is")

;;; AWS Setup
(defparameter *aws-region* "eu-west-1")
(setf zs3:*s3-endpoint* (concatenate 'string "s3-" *aws-region* ".amazonaws.com"))
(setf zs3:*s3-region* *aws-region*)
(setf zs3:*use-ssl* t)
(setf zs3:*signed-payload* nil)

(defclass environment-credentials () ())

(defmethod access-key ((credentials environment-credentials))
  (declare (ignore credentials))
  (asdf::getenv "AWS_ACCESS_KEY"))

(defmethod secret-key ((credentials environment-credentials))
  (declare (ignore credentials))
  (asdf::getenv "AWS_SECRET_KEY"))

(defmethod loaded ((credentials environment-credentials))
  (and (not (eq (access-key credentials) nil))
       (not (eq (secret-key credentials) nil))))

(defmethod print-object ((credentials environment-credentials) out)
  (print-unreadable-object (credentials out :type t)
    (if (loaded credentials)
	(format out "Environment credentials ~A:~A" (access-key credentials) (secret-key credentials))
	(format out "Empty credentials (envvars not set up)"))))

(defvar *credentials* nil  "AWS credentials")
(setf *credentials* (make-instance 'environment-credentials))

(defvar *dyna*
  (if (and *credentials*
	   (loaded *credentials*))
      (dyna:make-dyna :credentials (cons (access-key *credentials*)
				    (secret-key *credentials*))
		 :region *aws-region*)
      (dyna:make-dyna :region "local"))
  "Local connection to dynamodb")

(defparameter *static-bucket* "rayslava-statics"
  "Name of AWS S3 bucket with static files")
