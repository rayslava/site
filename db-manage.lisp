(defpackage :site.db-manage
  (:use :cl :site :site.config :site.db-storage :hunchentoot :cl-who :jonathan :dyna))

(in-package :site.db-manage)
(setf (html-mode) :html5)

(defun format-link (name)
  "Generate the s3 link to required `name'"
  (concatenate 'string "https://" zs3:*s3-endpoint* "/" *static-bucket* "/" name))

(defun beautify-attrs (attrs)
  "Parse the attrs of `attrs' and print the beautiful HTML"
  (let ((attrlist (parse attrs)))
    (with-html-output-to-string (*standard-output* nil :prologue nil)
      (:b "Tags: ")
      (mapcar #'(lambda (e) (fmt "~a " e))
	      (getf attrlist :tags)))))

(define-easy-handler (image :uri "/img"
			    :default-request-type :get)
    (name)
  (let* ((s3obj (car (select-dyna 'static-file
				  (sxql:where
				   (:= :s3name (compute-s3-name name))))))
	 (link (format-link (s3name s3obj))))
    (redirect link)))

(define-easy-handler (staticlist :uri "/admin/statics"
				 :default-request-type :get)
    ()
  (with-http-authentication
      (with-html-output-to-string (*standard-output* nil :prologue t)
	(:html
	 (:head (:title "List of available static files")
		(:link :rel "stylesheet" :type "text/css" :href "/main.css")
		(:script :type "text/javascript" :src "/jscl.js"))
	 (:body (:h2 "Statics")
		(:table
		 (:tr (:td "Filename") (:td "Attrs"))
		 (mapcar (lambda (e)
			   (htm (:tr (:td
				      (:a :href (format nil "/img?name=~A" (filename e))
					  (fmt "~A" (filename e))))
				     (:td (fmt "~A" (beautify-attrs (attr e)))))))
			 (site.db-storage:list-available-statics))))))))

(define-easy-handler (upload-req :uri "/admin/upload"
				 :default-request-type :get)
    ()
  (with-http-authentication
      (with-html-output-to-string (*standard-output* nil :prologue t)
	(:html
	 (:head (:title "Upload static files")
		(:link :rel "stylesheet" :type "text/css" :href "/main.css")
		(:script :type "text/javascript" :src "/jscl.js"))
	 (:body (:h2 "Upload")
		(:p (:form :id "uploadform"
			   :method :post
			   :enctype "multipart/form-data"
			   (:table
			    :border 0 :cellpadding 5 :cellspacing 0 :width "100%"
			    (:tr (:td :style "text-align: right" (str "Tags:"))
				 (:td :colspan 2 (:input :type :text :id "tags" :name "tags" :style "width:100%")))
			    (:tr (:td :style "text-align: right" (str "File:"))
				 (:td (:input :type :file :name "uploaded" :id "uploaded"))
				 (:td (:input :type :button :id "send" :value "Send" :disabled "true" :style "width:85%")))
			    (:tr (:td :style "text-align: right" (str "Progress:"))
				 (:td :colspan 2
				      (:div :id "progress" :style "width:100%"
					    (:div :id "bar" :style "text-align:center;width:1%;height=16px;background-color:#DCDCDC;"))))))))

	 (:script :type "text/javascript"
		  "document.getElementById('send').addEventListener('click', function(e) {
    var uploaded = document.getElementById('uploaded').files[0];
    var tags = document.getElementById('tags').value;
    var fd = new FormData();
    fd.append('uploaded', uploaded);
    fd.append('tags', tags);
    var xhr = new XMLHttpRequest();
    (xhr.upload || xhr).addEventListener('progress', function(e) {
        var done = e.position || e.loaded
        var total = e.totalSize || e.total;
        document.getElementById('bar').style.width = Math.round(done/total*100) + '%';
        document.getElementById('bar').innerHTML = Math.round(done/total*100) + '%';
    });
    xhr.addEventListener('load', function(e) {
        document.getElementById('send').disabled = 0;
    });
    xhr.open('POST', '/admin/do-upload', true);
    document.getElementById('send').disabled = 1;
    xhr.send(fd);
});")
	 (:script :type "text/x-common-lisp" "(setf (cl::oget (#j:document:getElementById \"send\") \"disabled\") 0)")))))


(define-easy-handler (upload-work :uri "/admin/do-upload")
    (uploaded tags)
  (print uploaded)
  (let ((attrs `(:tags ,(mapcar
			 (lambda (s)
			   (string-trim '(#\Space #\Newline #\Backspace #\Tab
					  #\Linefeed #\Page #\Return #\Rubout)
					s))
			 (split-sequence:split-sequence #\, tags))
		       :filename ,(cadr uploaded))))
    (print (format nil "Uploading: ~a ~a" (car uploaded) attrs))
    (upload-file (car uploaded) attrs)))
