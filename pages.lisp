					; Here are "static" pages
(defpackage :site.pages
  (:use :cl :hunchentoot :cl-who :ht-simple-ajax
	:asdf :site))
					;  (:export :generate-pages))

(in-package :site.pages)
(setf (html-mode) :html5)

					; Generates an administration page
(define-easy-handler (admin :uri "/admin"
			    :default-request-type :get)
    ((action :parameter-type 'string))
  (with-http-authentication
      (with-html-output-to-string (*standard-output* nil :prologue t)
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
					   "Recompile and reload .lisp files"))
				  (:li (:a :href "/admin/statics"
					   "List of available static files in S3"))
				  (:li (:a :href "/admin/upload"
					   "Upload new file into S3 storage"))))))
			  ((equalp action "pull")
			   (with-html-output (*standard-output* nil)
			     (:p "Pull result: "
				 (str (sh "git pull")))
			     (:a :href "/admin?action=list" "back to list")))
			  (t (with-html-output (*standard-output* nil)
			       (:a :href "/admin?action=list" "List actions"))))))))))

					; Main page goes here
(define-easy-handler (main-page :uri "/main"
                                :default-request-type :get)
    ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "About site")
	    (:link :rel "stylesheet" :type "text/css" :href "/main.css")
	    (:link :rel "alternate"  :type "application/rss+xml" :title "rayslava" :href "/rss")
	    (:script :type "text/javascript" :src "/jscl.js")
	    (:meta :http-equiv "Content-Type" :content "text/html; charset=utf-8")
	    (:meta :name "viewport" :content "initial-scale=1.0,maximum-scale=1.0,width=device-width,user-scalable=0"))
     (:body (:h2 "About site")
	    (:p "This site is just my first attempt to build the whole site using only a stack of LISP technologies.")
	    (:ul
	     (:li (:a :href "http://weitz.de/cl-who" "cl-who"))
	     (:li (:a :href "http://github.com/Inaimathi/cl-css" "cs-css"))
	     (:li (:a :href "http://github.com/davazp/jscl" "jscl")))
	    (:p
	     "It works on " (:a :href "http://weitz.de/hunchentoot" "hunchentoot")
	     " under " (:a :href "http://ecls.sourceforge.net" "ecl")
	     " on " (:a :href "http://gentoo.org" "Gentoo Linux")
	     " installed on " (:a :href "http://www.hardkernel.com" "ODroid U2") ".")
	    (:p "Now I'm working on migration to AWS and preparing the docker-compose image though.")
	    (:p "If you are courious how it's made, you are free to look through sources at " (:a :href "http://github.com/rayslava/site" "github") ".")
	    (:p "I also created an almost useful page with my contacts at " (:a :href "/contacts" "/contacts") " :)")))))

(define-easy-handler (robots-page :uri "/robots.txt"
				  :default-request-type :get)
    ()
  (string "Host: rayslava.com
User-agent: *
Allow: /main
Allow: /contacts
Allow: /blog
"))

(define-easy-handler (contacts-page :uri "/contacts"
				    :default-request-type :get)
    ()
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title "Contacts")
	    (:link :rel "stylesheet" :type "text/css" :href "/main.css")
	    (:meta :http-equiv "Content-Type" :content "text/html; charset=utf-8")
	    (:script :type "text/javascript" :src "/jscl.js")
	    (:script :type "text/javascript" "!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');")
	    (:script :type "text/x-common-lisp" "(setf (cl::oget (#j:document:getElementById \"mail-addr\") \"innerHTML\") \"<a href=\\\"mailto:rayslava@gmail.com\\\"> rayslava@gmail.com<\/a>\")")
	    (:meta :name "viewport" :content "initial-scale=1.0,maximum-scale=1.0,width=device-width,user-scalable=0"))
     (:body (:h2 "Contacts")
	    (:p "If you want to contact me you may want to:"
		(:ul
		 (:li "Write me a letter to my GMail: " (:span :id "mail-addr" (:b "rayslava") (:em "[at]") (:b "gmail.com"))
		      (:p "By the way you can use PGP to write me something personal. Public key can be found at " (:a :href "http://pgp.mit.edu:11371/pks/lookup?search=rayslava&op=index" "pgp.mit.edu")))
		 (:li "Chat me via jabber: "
		      (:a :href "xmpp:rayslava@rayslava.com?message;type=chat" "rayslava@rayslava.com"))
		 (:li "Follow my personal " (:a :href "http://point.im" "Point.im") " blog at "
		      (:a :href "xmpp:point@point.im?message;type=chat;body=S%20@rayslava" "point.im"))))
	    (:p "Also here are several social profiles:"
		(:div :class "social-link"
		      (:iframe
		       :src "https://ghbtns.com/github-btn.html?user=rayslava&type=follow&count=true"
		       :allowtransparency "true" :frameborder "0" :scrolling "0"
		       :width "164"
		       :height "20"))
		(:div :class "social-link"
		      (:a :href "https://twitter.com/Rayslava" :class "twitter-follow-button"
			  :data-show-count "true" "Follow @Rayslava"))
		(:div :class "social-link"
		      (:a :href "https://linkedin.com/in/rayslava"
			  (:img :src "https://static.licdn.com/scds/common/u/img/webpromo/btn_profile_greytxt_80x15.png"
				:width "80" :height "15" :border "0" :alt "View my LinkedIn profile")))
		(:div :class "social-link"
		      (:iframe :src "https://www.facebook.com/plugins/follow.php?href=https%3A%2F%2Fwww.facebook.com%2Frayslava&amp;layout=button_count&amp;show_faces=false&amp;colorscheme=light&amp;font&amp;width=164"
			       :scrolling "0" :frameborder "0"
			       :width "120" :height "20" :allowTransparency "true")))))))
