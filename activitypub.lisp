(defpackage :site.activitypub
  (:use :cl :hunchentoot :cl-who :cl-json
	:asdf :site :site.db-manage :site.config))

(in-package :site.activitypub)

					; Accept followers
(define-easy-handler (actor :uri "/ap/actor/blog/inbox"
			    :default-request-type :post)
    ()
  (setf (hunchentoot:content-type*) "application/ld+json")
  (let ((cl-json::+json-lisp-escaped-chars+
	  (remove #\/ cl-json::+json-lisp-escaped-chars+ :key #'car)))

    (let ((request-type (hunchentoot:request-method hunchentoot:*request*)))
      (cond ((eq request-type :get) "" );; handle get request
            ((eq request-type :post)
             (let* ((data-string (hunchentoot:raw-post-data :force-text t))
                    (json-obj (cl-json:decode-json-from-string data-string))) ;; use jsown to parse the string
	       (print (format nil "JSON: ~A"json-obj))
	       ""))))))
