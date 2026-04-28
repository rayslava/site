;;; RSS feed generation.
;;;
;;; Lives in its own file because cl-who:html-mode is a compile-time
;;; global flag, so the :xml flip has to be bracketed around a compilable
;;; unit. Isolating the flip here keeps the rest of the codebase free of
;;; side-channel compile-time mode toggles.

(defpackage :site.rss
  (:use :cl :cl-who :local-time :site.blog-post :site.blog-registry)
  (:export :build-rss-feed))

(in-package :site.rss)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (cl-who:html-mode) :xml))

(defun build-rss-feed (posts &key (title "rayslava's blog")
                                  (link "http://rayslava.com")
                                  (description "Blog feed"))
  "Render POSTS as an RSS 2.0 feed string. Pure: no *request*, no globals."
  (with-html-output-to-string (s nil :prologue "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" :indent nil)
    (:rss :|version| "2.0"
          (:channel
           (:title (str title))
           (:link (str link))
           (:description (str description))
           (dolist (post posts)
             (let ((post-title (subject post))
                   (post-link (format nil "~A/blog?id=~A" link (id post)))
                   (render-body (post post)))
               (htm (:item
                     (htm (:title (str post-title))
                          (:link (str post-link))
                          (:pubDate (str (format-timestring
                                          nil (universal-to-timestamp (id post))
                                          :format +rfc-1123-format+)))
                          (:description (str (funcall render-body))))))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (cl-who:html-mode) :html5))
