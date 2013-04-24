;; Main web-server file
;; (declaim (optimize (debug 3)))

(defpackage :piserv
  (:use :cl :hunchentoot :cl-who :ht-simple-ajax
	:asdf :piserv.static)
  (:export :start-server :stop-server :refresh :sh
	   :with-http-authentication :*ajax-processor*))

(in-package :piserv)

(defvar *hunchentoot-server* nil
  "Hunchentoot server instance")

(defvar *admin-login* "admin"
  "Admin page password")

(defvar *admin-password* "adminpassword"
  "Admin page password")

;;;;; First we create an ajax processor that will handle our function calls
(defvar *ajax-processor* 
  (make-instance 'ajax-processor :server-uri "/ajax"))

;;;;; Now we can define a function that we want to call from a web
;;;;; page. This function will take 'name' as an argument and return a
;;;;; string with a greeting.
(defun-ajax say-hi (name) (*ajax-processor*)
  (concatenate 'string "After server processing string is still " name))

;; Handler functions either return generated Web pages as strings,
;; or write to the output stream returned by write-headers

(defun sh (cmd)
  "A compiler-wide realization of running a shell command"
  (let ((in 
	 #+clisp (shell cmd)
	 #+ecl (two-way-stream-input-stream
		(ext:run-program "/bin/sh" (list "-c" cmd)
				 :input nil :output :stream :error :output))
	 #+sbcl (sb-ext:process-output
		 (sb-ext:run-program "/bin/sh" (list "-c" cmd)
				     :input nil :output :stream :error :output))
	 #+clozure (two-way-stream-input-stream
		    (ccl:run-program "/bin/sh" (list "-c" cmd) :
				     input nil :output :stream :error :output))))
    (with-output-to-string (s)
      (loop for line = (read-line in nil)
	 while line do (format s "~a<br />~%" line))
      s)))

(defun setup-dispatch-table ()
  "Set up dispatch table with file handlers for hunchentoot"
  (setq *dispatch-table*        
	(concatenate 'list
		     (piserv.static:generate-static-table)
		     (list
		      'dispatch-easy-handlers
		      (create-ajax-dispatcher *ajax-processor*)
		      ;; catch all
		      (lambda (request)
			(declare (ignore request))
			(redirect "/main"))))))

(defun stop-server ()
  "Stops the server"
  (when *hunchentoot-server*
    (stop *hunchentoot-server*)
    (setq *hunchentoot-server* nil)))

(defun start-server (&optional (port 8080) (adminpass "adminpassword"))
  "Starts the server"
  (when *hunchentoot-server*
    (stop-server))
  (setup-dispatch-table)
  (setf *admin-password* adminpass)
  (setq *hunchentoot-server*
	(start (make-instance 'easy-acceptor :port port))))

(defmacro with-http-authentication (&rest body)
  `(multiple-value-bind (username password) (hunchentoot:authorization)
     (cond ((and (string= username *admin-login*) (string= password *admin-password*))
            ,@body)
           (t (hunchentoot:require-authorization "admin-login")))))

(defun refresh ()
  "This function should be used by user for regenerating caches"
  (with-html-output (*standard-output* nil)
    (let ((in (make-string-input-stream
	       (with-output-to-string (*standard-output* nil)
		 (compile-file "static.lisp")
		 (load "static.lisp")
		 (compile-file "pages.lisp")
		 (load "pages.lisp")
		 (compile-file "site.lisp")
		 (load "site.lisp")
		 (setup-dispatch-table))))
	  (s (make-array '(0) :element-type 'base-char
			 :fill-pointer 0 :adjustable t)))
      (loop for line = (read-line in nil)
	 while line do (format s "~a<br />~%" line))
      (str s))))
