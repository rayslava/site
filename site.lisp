;; Main web-server file
;; (declaim (optimize (debug 3)))

(defpackage :site
  (:use :cl :hunchentoot :cl-who :ht-simple-ajax
	:asdf :site.static :site.config :site.db-manage)
  (:export :start-server :stop-server :refresh :sh
	   :*ajax-processor* :say-hi))

(in-package :site)

(setf (html-mode) :html5)

(defvar *hunchentoot-server* nil
  "Hunchentoot server instance")

;;;;; First we create an ajax processor that will handle our function calls
(defvar *ajax-processor*
  (make-instance 'ajax-processor :server-uri "/ajax"))

;;;;; Now we can define a function that we want to call from a web
;;;;; page. This function will take 'name' as an argument and return a
;;;;; string with a greeting.
(defun-ajax say-hi (name) (*ajax-processor*)
  (format nil "~A" (concatenate 'string "After server processing string is still " name)))

(push (ht-simple-ajax:create-ajax-dispatcher *ajax-processor*) hunchentoot:*dispatch-table*)

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
		     (site.static:generate-static-table)
		     (list
		      'dispatch-easy-handlers
		      (create-ajax-dispatcher *ajax-processor*)
		      ;; catch all
		      (lambda (request)
			(declare (ignore request))
			(redirect "/about" :host "rayslava.com" :protocol :https)))))
  (init-static-handlers))

(defun stop-server ()
  "Stops the server"
  (when *hunchentoot-server*
    (stop *hunchentoot-server*)
    (setq *hunchentoot-server* nil)))

(defun start-server (&optional (port *server-port*) (adminpass *admin-password*))
  "Starts the server"
  (when *hunchentoot-server*
    (stop-server))
  (setup-dispatch-table)
  (setf *admin-password* adminpass)
  (setf *log-lisp-errors-p* t)
  (setf *log-lisp-backtraces-p* t)
  (setf *log-lisp-warnings-p* t)
  (setf *lisp-warnings-log-level* :info)
  (setq *hunchentoot-server*
	(start (make-instance 'easy-acceptor :port port
			      :access-log-destination *access-log-file*
			      :message-log-destination *message-log-file*))))

(defun refresh ()
  "This function should be used by user for regenerating caches"
  (with-html-output (*standard-output* nil)
    (let ((in (make-string-input-stream
	       (with-output-to-string (*standard-output* nil)
		 (asdf:operate 'compile-op :site)
		 (asdf:operate 'load-op :site)
		 (setup-dispatch-table))))
	  (s (make-array '(0) :element-type 'base-char
			 :fill-pointer 0 :adjustable t)))
      (loop for line = (read-line in nil)
	 while line do (format s "~a<br />~%" line))
      (str s))))
