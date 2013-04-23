;; Main web-server file
;; (declaim (optimize (debug 3)))

(defpackage :piserv.main
  (:use :cl :hunchentoot :cl-who :ht-simple-ajax
	:asdf :piserv.static)
  (:export :start-server :stop-server :refresh))

(in-package :piserv.main)

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
  (let ((in 
	 #+clisp (shell cmd)
	 #+ecl (two-way-stream-input-stream (ext:run-program "/bin/sh" (list "-c" cmd) :input nil :output :stream :error :output))
	 #+sbcl (sb-ext:process-output (sb-ext:run-program "/bin/sh" (list "-c" cmd) :input nil :output :stream :error :output))
	 #+clozure (two-way-stream-input-stream (ccl:run-program "/bin/sh" (list "-c" cmd) :input nil :output :stream :error :output))))
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

(define-easy-handler (admin :uri "/admin"
			    :default-request-type :get)
    ((action))
  (with-http-authentication
      (with-html-output-to-string (*standard-output* nil :prologue nil)
	(:html
	 (:head (:title "Admin page")
		(:link :rel "stylesheet" :type "text/css" :href "/main.css")
		(:script :type "text/javascript" :src "/ecmalisp.js"))
	 (:body (:h2 "Admin interface")
		(:p (cond ((equalp action "refresh")
			   (with-html-output (*standard-output* nil)
			     (refresh)
			     (:h4 "Handlers refreshed")
			     (:a :href "/admin?action=list" "back to list")))
			  ((equalp action "list")
			   (with-html-output (*standard-output* nil)
			     (:h3 "Actions:")
			     (:p (:ul
				  (:li (:a :href "/admin?action=list" "list"))
				  (:li (:a :href "/admin?action=pull" "pull"))
				  (:li (:a :href "/admin?action=refresh" "refresh"))))))
			  ((equalp action "pull")
			   (with-html-output (*standard-output* nil)
			     (:p "Pull result: "
				 (str (sh "git pull")))
			     (:a :href "/admin?action=list" "back to list"))))))))))
  
(define-easy-handler (easy-demo :uri "/main"
                                :default-request-type :get)
    ((state-variable :parameter-type 'string))
  (with-html-output-to-string (*standard-output* nil :prologue nil)
    (:html
     (:head (:title "Hello, world!")
	    (:link :rel "stylesheet" :type "text/css" :href "/main.css")
	    (:script :type "text/javascript" :src "/ecmalisp.js")
	    (princ (generate-prologue *ajax-processor*))
	    (:script :type "text/javascript" "
function pv(x) { return x==undefined? nil: x; }

lisp.write = function(str){
           document.getElementById('eval').innerHTML = str;
           return str;
        }
lisp.evalString(pv, '(CL:PACKAGE-NAME CL:*PACKAGE*)');

// will show the greeting in a message box
function callback(response) {
  document.getElementById('answer').innerHTML = response.firstChild.firstChild.nodeValue;
}

// calls our Lisp function with the value of the text field
function sayHi() {
  lisp.write(lisp.evalString(pv,document.getElementById('name').value));
  ajax_say_hi(document.getElementById('name').value, callback);
}
")))
    (:body
     (:h1 "Hello")
     (:p "This is my Lisp web server, running on Hunchentoot,")
     (:p "Please enter lisp string: " 
	 (:input :id "name" :type "text"))
     (:p "Answer zone: " (:div :id "answer") (:div :id "eval"))
     (:p (:a :href "javascript:sayHi()" "Input"))
     (:b state-variable))))

(defun refresh ()
  "This function should be used by user for regenerating caches"
  (with-html-output (*standard-output* nil)
    (let ((in (make-string-input-stream
	       (with-output-to-string (*standard-output* nil)
		 (compile-file "static.lisp")
		 (load "static.lisp")
		 (compile-file "site.lisp")
		 (load "site.lisp")
		 (setup-dispatch-table))))
	  (s (make-array '(0) :element-type 'base-char
			 :fill-pointer 0 :adjustable t)))
      (loop for line = (read-line in nil)
	 while line do (format s "~a<br />~%" line))
      (str s))))
