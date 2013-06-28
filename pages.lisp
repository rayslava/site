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
		(:script :type "text/javascript" :src "/jscl.js"))
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
			     (:a :href "/admin?action=list" "back to list")))
			  (t (with-html-output (*standard-output* nil)
			       (:a :href "/admin?action=list" "List actions"))))))))))

; Main page goes here
(define-easy-handler (easy-demo :uri "/main"
                                :default-request-type :get)
    ((state-variable :parameter-type 'string))
  (with-html-output-to-string (*standard-output* nil :prologue nil)
    (:html
     (:head (:title "rayslava's test page")
	    (:link :rel "stylesheet" :type "text/css" :href "/main.css")
	    (:script :type "text/javascript" :src "/x-cl.js")
	    (:script :type "text/javascript" :src "/jscl.js")
	    (:meta :name "viewport" :content "initial-scale=1.0,maximum-scale=1.0,width=device-width,user-scalable=0;"))
	    (princ (generate-prologue *ajax-processor*))
	    (:script :type "text/javascript" "
document.addEventListener( \"DOMContentLoaded\", parselisp, false )

lisp.write = function(str){
           document.getElementById('eval').innerHTML = str;
           return str;
        }
lisp.evalString('(CL:PACKAGE-NAME CL:*PACKAGE*)');

// will show the greeting in a message box
function callback(response) {
  document.getElementById('answer').innerHTML = response.firstChild.firstChild.nodeValue;
}

// calls our Lisp function with the value of the text field
function sayHi() {
  lisp.write(lisp.evalString(document.getElementById('name').value));
  ajax_say_hi(document.getElementById('name').value, callback);
}
")
    (:body
     (:h1 "Hello")
     (:p "This is my Lisp web server, running on Hunchentoot,")
     (:p "For now you can take a look on my " (:a :href "/about" "personal page"))
     (:p "Or try to evaluate a lisp expression on "
	 (:a :href "https://github.com/davazp/jscl" "jscl") ":"
	 (:input :id "name" :type "text"))
     (:p "Answer zone: " (:div :id "answer") (:div :id "eval"))
     (:p (:a :href "javascript:sayHi()" "Input"))
     (:b (str state-variable))))))

(define-easy-handler (contacts-page :uri "/contacts"
				 :default-request-type :get)
    ()
  (with-html-output-to-string (*standard-output* nil :prologue nil)
    (:html
     (:head (:title "Contacts")
	    (:link :rel "stylesheet" :type "text/css" :href "/main.css")
	    (:script :type "text/javascript" """
function loaded() {
document.getElementById('mail-addr').innerHTML = '<a href=\\\"mailto:rayslava@gmail.com\\\"> rayslava@gmail.com<\/a>';
};
""")
	    (:script :type "text/javascript" :src "/x-cl.js")
	    (:script :type "text/javascript" :src "/jscl.js")
	    (:script :type "text/x-common-lisp" "(setf (#j:document:getElementById 'mail-addr'):innerHTML = '<a href=\\\"mailto:rayslava@gmail.com\\\"> rayslava@gmail.com<\/a>'")
;                                         
	    (:meta :name "viewport" :content "initial-scale=1.0,maximum-scale=1.0,width=device-width,user-scalable=0"))
     (:body (:h2 "Contacts")
	    (:p "If you want to contact me you may want to:"
		(:ul
		 (:li "Write me a letter to my GMail: " (:span :id "mail-addr" (:b "rayslava") (:em "[at]") (:b "gmail.com"))
		      (:p "By the way you can use PGP to write me something personal. Public key can be found at " (:a :href "http://pgp.mit.edu:11371/pks/lookup?search=rayslava&op=index" "pgp.mit.edu")))
		 (:li "Connect me in chat at jabber: "
		      (:a :href "xmpp:rayslava@jabber.ru?message;type=chat" "rayslava@jabber.ru"))
		 (:li "Follow me at personal jabber blog at "
		      (:a :href "xmpp:point@point.im?message;type=chat;body=S%20@rayslava" "point.im"))))))))

(define-easy-handler (about-page :uri "/about"
				 :default-request-type :get)
    ()
  (with-html-output-to-string (*standard-output* nil :prologue nil)
    (:html
     (:head (:title "About me")
	    (:link :rel "stylesheet" :type "text/css" :href "/main.css")
	    (:script :type "text/javascript" :src "/jscl.js")
	    (:script "!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');")
	    (:meta :name "viewport" :content "initial-scale=1.0,maximum-scale=1.0,width=device-width,user-scalable=0;"))
     (:body (:h2 "About me")
	    (:p "I was born on the day when Morris Worm devoured ten percent of Internet and that predetermined my life&hellip;")
	    (:p "I've graduated from BMSTU and now I work in Samsung Electronics Moscow")
	    (:p "Several social profiles:"
		(:div :class "social-link"
		      (:iframe
		       :src "http://ghbtns.com/github-btn.html?user=rayslava&type=follow&count=true"
		       :allowtransparency "true" :frameborder "0" :scrolling "0"
		       :width "164"
		       :height "20"))
		(:div :class "social-link"
		      (:a :href "https://twitter.com/Rayslava" :class "twitter-follow-button"
			  :data-show-count "true" "Follow @Rayslava"))
		(:div :class "social-link"
		      (:iframe :src "http://www.facebook.com/plugins/follow.php?href=https%3A%2F%2Fwww.facebook.com%2Frayslava&amp;layout=button_count&amp;show_faces=false&amp;colorscheme=light&amp;font&amp;width=164"
			       :scrolling "0" :frameborder "0"
			       :width "120" :height "20" :allowTransparency "true")))))))
