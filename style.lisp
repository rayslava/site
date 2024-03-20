(defpackage :site.styles
  (:use :cl :hunchentoot :cl-css
	:asdf :site))

(in-package :site.styles)

(define-easy-handler (main :uri "/main.css"
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
      :transition-timing-function "linear")
     ("a:hover" :color "#c00")

     ("@media screen and (max-width: 720px)"
      ("body"
       :padding "1em"	:font-size "120%"
       :width "100%"   :max-width "100%"
       :box-sizing "border-box")
      (".social-link"
       :padding "2px"
       :display "block"))
     ("h1" :margin "0.5em 0" :font-size "200%")
     ("p" :margin "0.5em 0")
     (".social-link" :padding "4px" :display "inline-block")
     ("hr" :border "none" :border-bottom "1px solid silver"))))

(define-easy-handler (blog :uri "/blog.css"
			   :default-request-type :get)
    ()
  (setf (hunchentoot:content-type*) "text/css")
  (css
   `(("#taglist" :padding "4px" :display "inline-block" :max-height "32px")
     (".tag" :padding "4px" :border "1px solid silver" :margin "4px"
	     :border-radius "8px")
     ("#apubinfo" :padding "4px" :font-size "80%" :float "right")
     ("#timeinfo" :padding "4px" :font-size "80%" :float "right")
     (".text-with-image span" :display "inline-block" :height "1em" :width "auto" :vertical-align "middle")
     (".text-with-image img" :height "100%" :width "auto" :display "block"))))
