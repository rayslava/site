;;; preview.lisp
;;
;; Requires HTML elements like:
;;   (a :class "hover-preview" :data-preview "large.jpg" "Hover me")
;; and an empty containers:
;;   (:div :id "preview-box" :class "preview")
;;   (:div :id "map-preview-box" :class "preview")


(let* ((+hover-margin+	      30)   ; px gap from cursor
       (+map-hide-delay-ms+   500)  ; delay before hiding the map box
       (+default-map-zoom+    14)   ; default OSM zoom
       (+coord-precision+     6))   ; decimal places for bbox/marker

  ;; Use macros so (setf (og ...)) stays a valid "place".
  (macrolet ((og  (o k) `(jscl::oget ,o ,k))
	     (st  (el)	`(og ,el "style"))
	     (ds  (el)	`(og ,el "dataset")))
    (labels
	((px  (n) (format nil "~Apx" n))
	 (num (s) (if (stringp s) (read-from-string s) s))

	 (show!	    (el) (setf (og (st el) "display") "block"))
	 (hide!	    (el) (setf (og (st el) "display") "none"))
	 (set-html! (el html) (setf (og el "innerHTML") html))

	 ;; Zoom → bbox delta (in degrees)
	 (zoom->delta (zoom)
	   (cond
	     ((member zoom '(17 18 19 20))  0.002)
	     ((member zoom '(15 16))	    0.004)
	     ((member zoom '(13 14))	    0.01)
	     (t				    0.02)))

	 ;; Build OSM embed URL with fixed decimal formatting
	 (embed-url (lat lng zoom)
	   (let* ((lat	 (num lat))
		  (lng	 (num lng))
		  (d	 (zoom->delta zoom))
		  (min-lng (- lng d))
		  (min-lat (- lat d))
		  (max-lng (+ lng d))
		  (max-lat (+ lat d)))
	     (flet ((f (x) (format nil "~,vs" +coord-precision+ x)))
	       (format nil
		       "https://www.openstreetmap.org/export/embed.html?bbox=~A,~A,~A,~A&layer=mapnik&marker=~A,~A"
		       (f min-lng) (f min-lat) (f max-lng) (f max-lat) (f lat) (f lng)))))

	 ;; Position BOX near (mx,my). If it would overflow right/bottom edges, flip.
	 (place-box (box mx my &key (margin +hover-margin+))
	   (let* ((vw (og #j:window "innerWidth"))
		  (vh (og #j:window "innerHeight"))
		  (bw (og box "offsetWidth"))
		  (bh (og box "offsetHeight"))
		  (left (if (> (+ mx margin bw) vw)
			    (- mx margin bw)
			    (+ mx margin)))
		  (top	(if (> (+ my margin bh) vh)
			    (- my margin bh)
			    (+ my margin))))
	     (let ((left (max 0 left)) (top (max 0 top)))
	       (setf (og (st box) "left") (px left)
		     (og (st box) "top")  (px top))))))

      ;; -------------------------
      ;; Image hover preview
      ;; -------------------------
      (let* ((preview (#j:document:getElementById "preview-box"))
	     (nodes   (#j:document:querySelectorAll ".hover-preview"))
	     (len     (og nodes "length")))
	(loop for i from 0 below len do
	  (let ((el (og nodes i)))
	    (setf (og el "onmouseenter")
		  (lambda (e)
		    (let* ((src (og (ds el) "preview"))
			   (mx	(og e "pageX"))
			   (my	(og e "pageY")))
		      (when src
			(set-html! preview (concatenate 'string "<img src=\"" src "\">"))
			(show! preview)
			(place-box preview mx my)))))
	    (setf (og el "onmousemove")
		  (lambda (e)
		    (place-box preview (og e "pageX") (og e "pageY"))))
	    (setf (og el "onmouseleave")
		  (lambda (e) (hide! preview))))))

      ;; -------------------------
      ;; OSM hover preview (iframe)
      ;; -------------------------
      (let* ((box	 (#j:document:getElementById "map-preview-box"))
	     (nodes	 (#j:document:querySelectorAll ".hover-map"))
	     (len	 (og nodes "length"))
	     (hide-timer nil))
	(setf (og box "onmouseenter")
	      (lambda (ev)
		(when hide-timer
		  (#j:window:clearTimeout hide-timer)
		  (setf hide-timer nil))))
	(setf (og box "onmouseleave")
	      (lambda (ev)
		(setf hide-timer
		      (#j:window:setTimeout (lambda () (hide! box))
					    +map-hide-delay-ms+))))
	(loop for i from 0 below len do
	  (let ((el (og nodes i)))
	    (setf (og el "onmouseenter")
		  (lambda (e)
		    (let* ((lat	 (num (og (ds el) "lat")))
			   (lng	 (num (og (ds el) "lng")))
			   (zoom (parse-integer (or (og (ds el) "zoom")
						    (write-to-string +default-map-zoom+))))
			   (src	 (embed-url lat lng zoom))
			   (mx	 (og e "pageX"))
			   (my	 (og e "pageY")))
		      (set-html! box
				 (concatenate 'string
					      "<iframe src=\"" src "\" loading=\"lazy\" "
					      "referrerpolicy=\"no-referrer-when-downgrade\" "
					      "style=\"width:100%;height:100%;border:0;display:block\"></iframe>"))
		      (show! box)
		      (place-box box mx my)
		      (when hide-timer
			(#j:window:clearTimeout hide-timer)
			(setf hide-timer nil)))))
	    (setf (og el "onmousemove")
		  (lambda (e)
		    (place-box box (og e "pageX") (og e "pageY"))))
	    (setf (og el "onmouseleave")
		  (lambda (e)
		    (setf hide-timer
			  (#j:window:setTimeout (lambda () (hide! box))
						+map-hide-delay-ms+))))))))))
