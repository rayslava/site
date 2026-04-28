;;; RSS feed generation.
;;;
;;; Lives in its own file because cl-who:html-mode is a compile-time
;;; global flag, so the :xml flip has to be bracketed around a compilable
;;; unit. Isolating the flip here keeps the rest of the codebase free of
;;; side-channel compile-time mode toggles.
;;;
;;; Per-request mode switching is not possible: html-mode is consumed at
;;; macroexpansion time by with-html-output-to-string, so toggling the
;;; flag at runtime has no effect on already-compiled bodies. The file-
;;; scoped flip here is the only mechanism cl-who exposes. Nested HTML
;;; bodies expanded under :html5 elsewhere (e.g. the lambda produced by
;;; defblogpost) are opaque strings by the time we see them — we have to
;;; treat them as external data and encode them for XML explicitly.

(defpackage :site.rss
  (:use :cl :cl-who :local-time :site.blog-post :site.blog-registry)
  (:export :build-rss-feed))

(in-package :site.rss)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (cl-who:html-mode) :xml))

(defun cdata-safe-split (s)
  "Return S with any occurrence of \"]]>\" split across two CDATA sections.
Per XML spec, \"]]>\" inside CDATA terminates the section, so we break
the string after \"]]\", close the section, open a new one, and continue
with the \">\". This is the standard workaround; real-world feeds do it
this way too. If S contains no \"]]>\", it is returned unchanged."
  (with-output-to-string (out)
    (loop with i = 0
          for j = (search "]]>" s :start2 i)
          do (if j
                 (progn
                   (write-string s out :start i :end (+ j 2))
                   (write-string "]]><![CDATA[" out)
                   (setf i (+ j 2)))
                 (progn
                   (write-string s out :start i)
                   (loop-finish))))))

(defun build-rss-feed (posts &key (title "rayslava's blog")
                                  (link "http://rayslava.com")
                                  (description "Blog feed"))
  "Render POSTS as an RSS 2.0 feed string. Pure: no *request*, no globals.

Element text is entity-escaped with cl-who:esc. Per-item descriptions —
which contain the HTML body of the post, full of void tags like <br>
and <img> that are not well-formed XML — are wrapped in CDATA. Tag
names that RSS 2.0 spells in camelCase (pubDate, guid) use pipe-quoted
keywords so cl-who does not downcase them."
  (with-html-output-to-string (s nil :prologue "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>" :indent nil)
    (:rss :|version| "2.0"
          (:channel
           (:title (esc title))
           (:link (esc link))
           (:description (esc description))
           (dolist (post posts)
             (let* ((post-title (subject post))
                    (post-link (format nil "~A/blog?id=~A" link (id post)))
                    (render-body (post post))
                    (body-html (funcall render-body)))
               (htm (:item
                     (htm (:title (esc post-title))
                          (:link (esc post-link))
                          (:|guid| :|isPermaLink| "true" (esc post-link))
                          (:|pubDate| (esc (format-timestring
                                            nil (universal-to-timestamp (id post))
                                            :format +rfc-1123-format+)))
                          (:description
                           (str "<![CDATA[")
                           (str (cdata-safe-split body-html))
                           (str "]]>")))))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (cl-who:html-mode) :html5))
