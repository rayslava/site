;; Entry point for the standalone executable built by contrib/Dockerfile

(defpackage :site.main
  (:use :cl)
  (:export :main))

(in-package :site.main)

(defun env (name &optional default)
  "Value of environment variable NAME, or DEFAULT when unset or empty."
  (let ((value (uiop:getenv name)))
    (if (and value (string/= value ""))
	value
	default)))

(defun env-int (name default)
  (let ((value (env name)))
    (if value (parse-integer value) default)))

(defun refresh-load-time-config ()
  "Some config values are computed when config.lisp is loaded, which for
the executable means at image build time. Recompute them for this run."
  (setf site.config:*admin-password* (env "ADMIN_PASSWORD" "adminpassword"))
  (setf site.config:*default-static-path*
	(concatenate 'string (namestring (sb-posix:getcwd)) "/" "static")))

(defun use-source-checkout ()
  "When started inside the git checkout (see contrib/entrypoint.sh), make
the admin refresh action recompile :site from it. Dependencies stay as
loaded in the image; fasls go to ../cache next to the checkout. With
SITE_LOAD_SOURCE set the checkout is ahead of the image and is loaded now."
  (let ((asd (probe-file "site.asd")))
    (when asd
      (dolist (name (asdf:already-loaded-systems))
	(unless (member name '("site" "site/tests") :test #'string=)
	  (asdf:register-immutable-system name)))
      (asdf:initialize-output-translations
       `(:output-translations
	 (t (,(merge-pathnames "../cache/" (uiop:getcwd)) :implementation))
	 :ignore-inherited-configuration))
      ;; Forget the build location, otherwise ASDF keeps using it
      (asdf:clear-system "site/tests")
      (asdf:clear-system "site")
      (asdf:load-asd asd)
      (when (env "SITE_LOAD_SOURCE")
	(format t "~&Loading site from ~A~%" (uiop:getcwd))
	(finish-output)
	(handler-case (asdf:load-system :site)
	  (error (e)
	    (format *error-output* "~&Loading the checkout failed, serving the image code: ~A~%" e)))))))

(defun start-slynk ()
  "Start slynk only when SLYNK_PORT is set. Listens on loopback unless
SLYNK_INTERFACE says otherwise; reach it with `docker exec` or a tunnel."
  (let ((port (env-int "SLYNK_PORT" nil)))
    (when port
      (let ((slynk::*loopback-interface* (env "SLYNK_INTERFACE" "127.0.0.1")))
	(slynk:create-server :port port :style :spawn :dont-close t)))))

(defun main ()
  (sb-ext:disable-debugger)
  ;; SIGTERM (docker stop) runs sb-ext:exit, which calls these hooks
  (push #'site:stop-server sb-ext:*exit-hooks*)
  (setf sb-ext:*exit-timeout* 5)
  (handler-case
      (progn
	(refresh-load-time-config)
	(use-source-checkout)
	(start-slynk)
	(site:start-server (env-int "SITE_PORT" 8080) site.config:*admin-password*)
	(format t "~&Site is listening on port ~A~%" (env-int "SITE_PORT" 8080))
	(finish-output)
	(loop (sleep 60)))
    (sb-sys:interactive-interrupt ()
      (site:stop-server)
      (sb-ext:exit :code 0 :abort t))))
