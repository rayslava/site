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
     (:head (:title "About site")
	    (:link :rel "stylesheet" :type "text/css" :href "/main.css")
	    (:script :type "text/javascript" :src "/x-cl.js")
	    (:script :type "text/javascript" :src "/jscl.js")
	    (:meta :name "viewport" :content "initial-scale=1.0,maximum-scale=1.0,width=device-width,user-scalable=0"))
     (:body (:h2 "About site")
	    (:p "This site is just my first attempt to build the whole site using only a stack of LISP technologies."
		(:ul
		 (:li (:a :href "http://weitz.de/cl-who" "cl-who"))
		 (:li (:a :href "http://github.com/Inaimathi/cl-css" "cs-css"))
		 (:li (:a :href "http://github.com/davazp/jscl" "jscl")))
		"It works on " (:a :href "http://weitz.de/hunchentoot" "hunchentoot")
		" under " (:a :href "http://ecls.sourceforge.net" "ecl")
		" on " (:a :href "http://archlinuxarm.org" "Arch Linux ARM")
		" installed on " (:a :href "http://www.raspberrypi.org" "Rasbberry Pi"))
	    (:p "All the source  will be placed on my github when it is ready and I'm not feeling myself guilty for injuring readers' sense of beauty.")
	    (:p "I also created an almost useful page with my contacts at " (:a :href "/contacts" "/contacts") " :)")))))

(define-easy-handler (contacts-page :uri "/contacts"
				 :default-request-type :get)
    ()
  (with-html-output-to-string (*standard-output* nil :prologue nil)
    (:html
     (:head (:title "Contacts")
	    (:link :rel "stylesheet" :type "text/css" :href "/main.css")
	    (:script :type "text/javascript" :src "/x-cl.js")
	    (:script :type "text/javascript" :src "/jscl.js")
	    (:script :type "text/x-common-lisp" "(setf (cl::oget (#j:document:getElementById \"mail-addr\") \"innerHTML\") \"<a href=\\\"mailto:rayslava@gmail.com\\\"> rayslava@gmail.com<\/a>\")")
	    (:meta :name "viewport" :content "initial-scale=1.0,maximum-scale=1.0,width=device-width,user-scalable=0"))
     (:body (:h2 "Contacts")
	    (:p "If you want to contact me you may want to:"
		(:ul
		 (:li "Write me a letter to my GMail: " (:span :id "mail-addr" (:b "rayslava") (:em "[at]") (:b "gmail.com"))
		      (:p "By the way you can use PGP to write me something personal. Public key can be found at " (:a :href "http://pgp.mit.edu:11371/pks/lookup?search=rayslava&op=index" "pgp.mit.edu")))
		 (:li "Chat me via jabber: "
		      (:a :href "xmpp:rayslava@jabber.ru?message;type=chat" "rayslava@jabber.ru"))
		 (:li "Follow my personal " (:a :href "http://point.im" "Point.im") " blog at "
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
