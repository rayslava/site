;; preview.lisp
;;
;; Requires HTML elements like:
;;   (a :class "hover-preview" :data-preview "large.jpg" "Hover me")
;; and an empty container:
;;   (:div :id "preview-box")

(let* ((preview (#j:document:getElementById "preview-box"))
       (nodes   (#j:document:querySelectorAll ".hover-preview"))
       (len     (jscl::oget nodes "length")))
  (labels ((px (n) (format nil "~Apx" n)))
    (loop for i from 0 below len do
      (let ((el (jscl::oget nodes i)))
        ;; mouseenter: show and inject img
        (setf (jscl::oget el "onmouseenter")
              (lambda (e)
                (let* ((ds  (jscl::oget el "dataset"))
                       (src (jscl::oget ds "preview")))
                  (setf (jscl::oget preview "innerHTML")
                        (concatenate 'string "<img src=\"" src "\">"))
                  (setf (jscl::oget (jscl::oget preview "style") "display")
                        "block"))))
        ;; mousemove: follow cursor
        (setf (jscl::oget el "onmousemove")
              (lambda (e)
                (setf (jscl::oget (jscl::oget preview "style") "left")
                      (px (+ (jscl::oget e "pageX") 15)))
                (setf (jscl::oget (jscl::oget preview "style") "top")
                      (px (+ (jscl::oget e "pageY") 15)))))
        ;; mouseleave: hide
        (setf (jscl::oget el "onmouseleave")
              (lambda (e)
                (setf (jscl::oget (jscl::oget preview "style") "display")
                      "none")))))))
