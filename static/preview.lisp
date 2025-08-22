;; preview.lisp
;;
;; Requires HTML elements like:
;;   (a :class "hover-preview" :data-preview "large.jpg" "Hover me")
;; and an empty container:
;;   (:div :id "preview-box")

;; hover-previews.lisp  — merged (image + OSM iframe)

(labels
    ((px (n) (format nil "~Apx" n))
     (num (s) (if (stringp s) (read-from-string s) s))
     (embed-url (lat lng zoom)
       (let* ((lat  (num lat))
	      (lng  (num lng))
	      (delta (case zoom
		       ((17 18 19 20) 0.002)
		       ((15 16)        0.004)
		       ((13 14)        0.01)
		       (t              0.02)))
	      (min-lng (- lng delta))
	      (min-lat (- lat delta))
	      (max-lng (+ lng delta))
	      (max-lat (+ lat delta)))
	 (format nil
		 "https://www.openstreetmap.org/export/embed.html?bbox=~S,~S,~S,~S&layer=mapnik&marker=~S,~S"
		 min-lng min-lat max-lng max-lat lat lng))))

  ;; -------------------------
  ;; Image hover preview
  ;; -------------------------
  (let* ((preview (#j:document:getElementById "preview-box"))
         (nodes   (#j:document:querySelectorAll ".hover-preview"))
         (len     (jscl::oget nodes "length")))
    (loop for i from 0 below len do
      (let ((el (jscl::oget nodes i)))
        ;; mouseenter: show and inject <img>
        (setf (jscl::oget el "onmouseenter")
              (lambda (e)
                (let* ((ds  (jscl::oget el "dataset"))
                       (src (jscl::oget ds "preview")))
                  (when src
                    (setf (jscl::oget preview "innerHTML")
                          (concatenate 'string "<img src=\"" src "\">"))
                    (setf (jscl::oget (jscl::oget preview "style") "display") "block")))))
        ;; mousemove: follow cursor
        (setf (jscl::oget el "onmousemove")
              (lambda (e)
                (let ((st (jscl::oget preview "style")))
                  (setf (jscl::oget st "left") (px (+ (jscl::oget e "pageX") 15))
                        (jscl::oget st "top")  (px (+ (jscl::oget e "pageY") 15))))))
        ;; mouseleave: hide
        (setf (jscl::oget el "onmouseleave")
              (lambda (e)
                (setf (jscl::oget (jscl::oget preview "style") "display") "none"))))))

  ;; -------------------------
  ;; OSM hover preview (iframe)
  ;; -------------------------
  (let* ((box        (#j:document:getElementById "map-preview-box"))
         (nodes      (#j:document:querySelectorAll ".hover-map"))
         (len        (jscl::oget nodes "length"))
         (hide-timer nil))
    ;; allow entering/leaving the map box without instant hide
    (setf (jscl::oget box "onmouseenter")
          (lambda (ev)
            (when hide-timer (#j:window:clearTimeout hide-timer) (setf hide-timer nil))))
    (setf (jscl::oget box "onmouseleave")
          (lambda (ev)
            (setf hide-timer
                  (#j:window:setTimeout
                   (lambda ()
                     (setf (jscl::oget (jscl::oget box "style") "display") "none"))
                   500))))

    (loop for i from 0 below len do
      (let ((el (jscl::oget nodes i)))
        ;; show map on enter
        (setf (jscl::oget el "onmouseenter")
              (lambda (e)
                (let* ((ds   (jscl::oget el "dataset"))
                       (lat  (num (jscl::oget ds "lat")))
                       (lng  (num (jscl::oget ds "lng")))
                       (zoom (parse-integer (or (jscl::oget ds "zoom") "14")))
                       (src  (embed-url lat lng zoom)))
                  (setf (jscl::oget box "innerHTML")
                        (concatenate 'string
				     "<iframe src=\"" src "\" loading=\"lazy\" "
				     "referrerpolicy=\"no-referrer-when-downgrade\" "
				     "style=\"width:100%;height:100%;border:0;display:block\"></iframe>"))
                  (let ((st (jscl::oget box "style")))
                    (setf (jscl::oget st "display") "block"
                          (jscl::oget st "left")    (px (+ (jscl::oget e "pageX") 15))
                          (jscl::oget st "top")     (px (+ (jscl::oget e "pageY") 15))))
                  (when hide-timer (#j:window:clearTimeout hide-timer) (setf hide-timer nil)))))
        ;; follow cursor
        (setf (jscl::oget el "onmousemove")
              (lambda (e)
                (let ((st (jscl::oget box "style")))
                  (setf (jscl::oget st "left") (px (+ (jscl::oget e "pageX") 15))
                        (jscl::oget st "top")  (px (+ (jscl::oget e "pageY") 15))))))
        ;; schedule hide when leaving the link (unless entering box)
        (setf (jscl::oget el "onmouseleave")
              (lambda (e)
                (setf hide-timer
                      (#j:window:setTimeout
                       (lambda ()
                         (setf (jscl::oget (jscl::oget box "style") "display") "none"))
                       500)))))))
