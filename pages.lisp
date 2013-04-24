; Here are "static" pages
(defpackage :piserv.pages
  (:use :cl :hunchentoot :cl-who :ht-simple-ajax
	:asdf :piserv))
;  (:export :generate-pages))

(in-package :piserv.pages)

; Generates an administration page
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
				  (:li (:a :href "/admin?action=list"
					   "List actions (this page)"))
				  (:li (:a :href "/admin?action=pull"
					   "Pull fresh from deploy"))
				  (:li (:a :href "/admin?action=refresh"
					   "Recompile and reload .lisp files"))))))
			  ((equalp action "pull")
			   (with-html-output (*standard-output* nil)
			     (:p "Pull result: "
				 (str (sh "git pull")))
			     (:a :href "/admin?action=list" "back to list"))))))))))

; Main page goes here
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
     (:b (str state-variable)))))

(define-easy-handler (about-page :uri "/about"
				 :default-request-type :get)
    ()
  (with-html-output-to-string (*standard-output* nil :prologue nil)
    (:html
     (:head (:title "Hello, world!")
	    (:link :rel "stylesheet" :type "text/css" :href "/main.css")
	    (:script :type "text/javascript" :src "/ecmalisp.js"))
     (:body (:h2 "About me")))))
	    
