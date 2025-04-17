(defpackage :site.styles
  (:use :cl :hunchentoot :cl-css
	:asdf :site))

(in-package :site.styles)

(define-easy-handler (maincss :uri "/main.css"
			      :default-request-type :get)
    ()
  (setf (hunchentoot:content-type*) "text/css")
  (css
   `(("body"
      :max-width "90%"
      :margin "auto"
      :padding "2em"
      :background "white"
      :color "#333"
      :font "100%/1.33 Serif"
      :text-align "justify")
     ("a"
      :color "#000"
      ,@(transition 'color :timing-function "ease-in" :duration 0.2)
      :transition-timing-function "linear"
      :text-decoration-thickness "0.3px;"
      :-webkit-text-decoration-thickness "0.3px;"
      :text-underline-offset "3px;"
      :-webkit-text-underline-offset "3px;")
     ("a:hover" :color "#c00")

     ("@media screen and (max-width: 720px)"
      ("body"
       :padding "1em"	:font-size "120%"
       :width "100%"   :max-width "100%"
       :box-sizing "border-box")
      (".social-link"
       :padding "2px"
       :display "block"))
     ("@media (prefers-color-scheme: dark)"
      ("body" :background "#1a1a1a" :color "#e0e0e0")
      ("a" :color "#bbb")
      ("a:hover" :color "#ff6666")
      ("hr" :border-bottom "1px solid #404040")
      ("h1, h2, h3, h4" :color "#e0e0e0"))
     ("h1" :margin "0.5em 0" :font-size "200%")
     ("p" :margin "0.5em 0")
     (".social-link" :padding "4px" :display "inline-block")
     ("hr" :border "none" :border-bottom "1px solid silver"))))

(define-easy-handler (blogcss :uri "/blog.css"
			      :default-request-type :get)
    ()
  (setf (hunchentoot:content-type*) "text/css")
  (css
   `(("@media (min-width: 768px)"
      ("html"
       :font-size "125%"
       :max-width "45em"
       :margin "0px auto"))
     ("h1, h2, h3, h4"
      :margin "2.5rem 0 1.5rem 0"
      :line-height "1.25"
      :color "#777")
     ("#taglist" :padding "4px" :display "inline-block" :max-height "32px")
     (".tag" :padding "4px" :border "1px solid silver" :margin "4px"
	     :border-radius "8px")
     ("#apubinfo" :padding "4px" :font-size "80%" :float "right")
     ("#timeinfo" :padding "4px" :font-size "80%" :float "right")
     (".text-with-image span" :display "inline-block" :height "1em" :width "auto" :vertical-align "middle")
     (".text-with-image img" :height "100%" :width "auto" :display "block")
     (".year-split" :border-bottom "1px solid;"
		    :border-image "linear-gradient(to right, #ccc, transparent) 1;"
		    :margin-bottom "0.3rem;"
		    :padding-bottom "0.2rem;")
     (".year-split h3" :margin "0;"
		       :color "#aaa;")
     ("@media (prefers-color-scheme: dark)"
      (".year-split h3" :color "#555;")
      ("h1, h2, h3, h4" :color "#999")))))

(define-easy-handler (logcss :uri "/log.css"
			     :default-request-type :get)
    ()
  (setf (hunchentoot:content-type*) "text/css")
  (css
   `(("body"
      :font-family "monospace"
      :background-color "#f0f0f0")
     (".log-container"
      :padding "10px"
      :background-color "#fff"
      :border-radius "5px")
     (".log-entry"
      :margin "5px 0"
      :padding "5px"
      :border-bottom "1px solid #eee")
     (".error"
      :color "red")
     (".info"
      :color "blue")
     (".access"
      :color "green")
     (".refresh-btn"
      :padding "10px"
      :background-color "#4CAF50"
      :color "white"
      :border "none"
      :cursor "pointer"
      :margin "10px 0"))))
