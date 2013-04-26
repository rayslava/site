; Here are css files
; Colors
; .primary-1 { background-color: #CCCCCC }
; .primary-2 { background-color: #999999 }
; .primary-3 { background-color: #333366 }
; .primary-4 { background-color: #CCCCCC }
; .primary-5 { background-color: #CCCCCC }

; .complement-1 { background-color: #FFFFFF }
; .complement-2 { background-color: #999999 }
; .complement-3 { background-color: #996633 }
; .complement-4 { background-color: #FFFFFF }
; .complement-5 { background-color: #FFFFFF }

(defpackage :piserv.styles
  (:use :cl :hunchentoot :cl-css
	:asdf :piserv))

(in-package :piserv.styles)

(define-easy-handler (admin :uri "/main.css"
			    :default-request-type :get)
()
  (css
   `(("body"
      :max-width "90%"
      :margin "auto"
      :padding "2em"
      :background "white"
      :color "#666"
      :font "75%/1.33 Serif"
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
      
