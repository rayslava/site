(defpackage :site.db-manage
  (:use :cl :hunchentoot :site.config :site.db-storage :hunchentoot :cl-who :jonathan :dyna)
  (:export :init-static-handlers :with-http-authentication))

(in-package :site.db-manage)
(setf (html-mode) :html5)

(defmacro with-http-authentication (&rest body)
  `(multiple-value-bind (username password) (hunchentoot:authorization)
     (cond ((and (string= username *admin-login*) (string= password *admin-password*))
            ,@body)
           (t (hunchentoot:require-authorization *admin-login-message*)))))

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

(defmacro defstatic-handler (url)
  "Create handler for static file stored in S3"
  `(lambda ()
     (let* ((full-url (request-uri* *request*))
	    (static-name (subseq full-url (length ,url)))
	    (s3objs (select-dyna 'static-file
				 (sxql:where
				  (:= :s3name (compute-s3-name static-name))))))
       (if (= (length s3objs) 1)
	   (redirect (format-link (s3name (car s3objs))))
	   (progn (setf (return-code*) +http-not-found+)
		  (abort-request-handler))))))

(defun init-static-handlers ()
  (mapcar (lambda (url) (push
			 (create-prefix-dispatcher url (defstatic-handler url))
			 *dispatch-table*))
	  '("/s/" "/i/" "/static/" "/image/" "/img/"))
  (print (format nil "~A ~A" "Static handlers created, current table: " *dispatch-table*)))

(define-easy-handler (staticlist :uri "/admin/statics"
				 :default-request-type :get)
    ()
  (with-http-authentication
      (no-cache)
    (with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
       (:head (:title "List of available static files")
	      (:link :rel "stylesheet" :type "text/css" :href "/main.css")
	      (:body (:h2 "Statics")
		     (:table
		      (:tr (:td "Filename") (:td "Attrs"))
		      (mapcar (lambda (e)
				(htm (:tr (:td
					   (:a :href (format nil "/i/~A" (filename e))
					       (fmt "~A" (filename e))))
					  (:td (fmt "~A" (beautify-attrs (attr e))))
					  (:td (:a :href (concatenate 'string "/admin/do-delete?victim=" (s3name e))
						   (fmt "~A ~A" "del " (filename e)))))))
			      (stable-sort
			       (site.db-storage:list-available-statics)
			       #'string-lessp
			       :key #'filename)))
		     (:script :type "text/javascript" :src "/jscl.js")))))))

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
			    (:tr (:td :style "text-align: right" (str "Files:"))
				 (:td (:input :type :file :name "uploaded" :id "uploaded" :multiple "multiple"))
				 (:td (:input :type :button :id "send" :value "Send" :disabled "true" :style "width:85%")))
			    (:tr (:td :style "text-align: right" (str "Progress:"))
				 (:td :colspan 2
				      (:div :id "progress" :style "width:100%"
					    (:div :id "bar" :style "text-align:center;width:1%;height=16px;background-color:#DCDCDC;")))))))

	 (:script :type "text/javascript"
		  "document.getElementById('send').addEventListener('click', function(e) {
    var uploadedFiles = document.getElementById('uploaded').files;
    var tags = document.getElementById('tags').value;
    var fd = new FormData();

    // Loop through all uploaded files
    for(var i = 0; i < uploadedFiles.length; i++) {
        fd.append('uploaded[]', uploadedFiles[i]);
    }

    fd.append('tags', tags);

    var xhr = new XMLHttpRequest();
    (xhr.upload || xhr).addEventListener('progress', function(e) {
        var done = e.position || e.loaded;
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

	 (:script :type "text/x-common-lisp" "(setf (jscl::oget (#j:document:getElementById \"send\") \"disabled\") 0)"))))))

(define-easy-handler (upload-work :uri "/admin/do-upload")
    (uploaded tags)
  (let* ((file-list (if (listp uploaded) uploaded (list uploaded)))
         (attrs `(:tags ,(mapcar
			  (lambda (s)
			    (string-trim '(#\Space #\Newline #\Backspace #\Tab
					   #\Linefeed #\Page #\Return #\Rubout)
					 s))
			  (split-sequence:split-sequence #\, tags))
			:filename ,(cadr (first file-list)))))
    (dolist (file file-list)
      (upload-file (car file) attrs))
    (hunchentoot:redirect "/admin/statics")))

(define-easy-handler (delete-work :uri "/admin/do-delete")
    ((victim :parameter-type 'string))
  (delete-static victim)
  (hunchentoot:redirect "/admin?action=list"))
