(defpackage :site.config
  (:use :cl :asdf)
  (:export :*admin-login*
	   :*admin-password*
	   :*access-log-file*
	   :*message-log-file*
	   :*default-static-path*
	   :*server-port*
	   :*admin-login-message*))

(in-package :site.config)

(defvar *admin-login* "admin"
  "Admin page password")

(defvar *admin-password* "adminpassword"
  "Admin page password")

(defvar *access-log-file* "access.log"
  "Server access log file")

(defvar *message-log-file* "message.log"
  "Server message log file")

(defvar *server-port* 8080
  "Default server port")

(defvar *admin-login-message* "Please enter admin credentials"
  "Message which will appear if user tries to get access to admin page")

(defvar *default-static-path* "static"
  "Default path where server should search for files that should be exported as is")

(defvar *dyna* (dyna:make-dyna :region "local")
  "Local connection to dynamodb")
