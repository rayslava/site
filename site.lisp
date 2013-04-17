;; Main web-server file
;; (declaim (optimize (debug 3)))

(defpackage :piserv.main
  (:use :cl :hunchentoot :cl-who :ht-simple-ajax
	:piserv.static)
  (:export :start-server :stop-server))

(in-package :piserv.main)
(defvar *hunchentoot-server* nil
  "Hunchentoot server instance")



;;;;; First we create an ajax processor that will handle our function calls
(defvar *ajax-processor* 
  (make-instance 'ajax-processor :server-uri "/ajax"))

;;;;; Now we can define a function that we want to call from a web
;;;;; page. This function will take 'name' as an argument and return a
;;;;; string with a greeting.
(defun-ajax say-hi (name) (*ajax-processor*)
  (concatenate 'string "After server processing string is still " name))

(defun-ajax lol () (*ajax-processor*)
  "OLOLOLO")


;; Handler functions either return generated Web pages as strings,
;; or write to the output stream returned by write-headers

(defun setup-dispatch-table ()
  (let ((css-name (make-pathname
		   :name "main" :type "css"
		   :version nil :defaults
		   (load-time-value
		    (or *load-pathname* #.*compile-file-pathname*))))
	(ecmalisp-name (make-pathname
			:name "ecmalisp" :type "js"
			:version nil :defaults
			(load-time-value
			 (or *load-pathname* #.*compile-file-pathname*)))))
    (print css-name)
    (print ecmalisp-name)
    (setq *dispatch-table*        
	  (concatenate 'list
		       (piserv.static:generate-static-table)
		       (list
			'dispatch-easy-handlers
			(create-ajax-dispatcher *ajax-processor*)
			;; catch all
			(lambda (request)
			  (declare (ignore request))
			  (redirect "/main")))))))

(defun stop-server ()
  "Stops the server"
  (when *hunchentoot-server*
    (stop *hunchentoot-server*)
    (setq *hunchentoot-server* nil)))

(defun start-server (&optional (port 8080))
  "Starts the server"
  (when *hunchentoot-server*
    (stop-server))
  (setup-dispatch-table)
  (setq *hunchentoot-server*
	(start (make-instance 'easy-acceptor :port port))))

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
