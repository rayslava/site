(defpackage :site.config
  (:use :cl :asdf :zs3 :dyna :alexandria :asn1 :trivia :cl-base64)
  (:export :*admin-login*
   :*admin-password*
	   :*access-log-file*
   :*message-log-file*
	   :*default-static-path*
   :*server-port*
	   :*admin-login-message*
   :environment-credentials
	   :*credentials*
   :*dyna*
	   :*static-bucket*
   :*aws-region*
	   :*distribution*
   :*activitypub-private-key*
	   :*activitypub-public-key-pem*
   :*activitypub-public-key*
   :load-activitypub-keys!
   :configure-aws!
   :initialize-dyna!
   :aws-credentials-available-p))

(in-package :site.config)

(defvar *admin-login* "admin"
  "Admin page login")

(defvar *admin-password*
  (let ((envpass (asdf::getenv "ADMIN_PASSWORD")))
    (if envpass
	envpass
	"adminpassword"))
  "Admin page password")

(defvar *access-log-file* *error-output*
  "Server access log file")

(defvar *message-log-file* *error-output*
  "Server message log file")

(defvar *server-port* 8080
  "Default server port")

(defvar *admin-login-message* "Please enter admin credentials"
  "Message which will appear if user tries to get access to admin page")

(defvar *default-static-path* (concatenate 'string (namestring (sb-posix::getcwd)) "/" "static")
  "Default path where server should search for files that should be exported as is")

;;; ActivityPub RSA keys — initialized lazily. Before
;;; load-activitypub-keys! is called these are NIL, so the rest of the
;;; system can load on a box without the PEM files (CI, dev laptop, tests).

(defvar *activitypub-private-key* nil
  "RSA private key used for signing ActivityPub HTTP requests.")

(defvar *activitypub-public-key-pem* nil
  "Raw PEM text of the ActivityPub public key, embedded in the actor JSON.")

(defvar *activitypub-public-key* nil
  "RSA public key used for publishing as the ActivityPub pubkey.")

(defun read-pem-body (filename)
  "Reads a PKCS#1 PEM body from FILENAME, stripping BEGIN/END delimiter lines."
  (with-open-file (stream filename)
    (apply 'concatenate 'string
	   (loop for line = (read-line stream nil)
		 while line
		 when (not (search "-----" line))
		   collect line))))

(defun generate-ephemeral-rsa-keypair (&key (n-bits 2048))
  "Generate a fresh in-memory RSA keypair for local dev without PEM files.
Returns (values priv pub pem-text). The pem-text is a minimal PKCS#1
wrapper around the DER-encoded public key."
  (multiple-value-bind (priv pub)
      (ironclad:generate-key-pair :rsa :num-bits n-bits)
    ;; No one-line PEM export in ironclad; leave the field blank — callers
    ;; that publish the actor JSON will see an empty publicKeyPem field.
    ;; Good enough for local smoke tests; real federation needs real keys.
    (values priv pub "")))

(defun load-activitypub-keys! (&key (dir nil) generate-if-missing)
  "Load RSA keys from DIR/private.pem and DIR/public.pem. If the files
do not exist and GENERATE-IF-MISSING is true, create an ephemeral
in-memory keypair instead (suitable for local dev only). DIR defaults
to the `activitypub/` subdirectory of the current working directory."
  (let* ((base (or dir (concatenate 'string (namestring (sb-posix::getcwd)) "/activitypub/")))
         (priv-path (concatenate 'string base "private.pem"))
         (pub-path  (concatenate 'string base "public.pem")))
    (cond
      ((and (probe-file priv-path) (probe-file pub-path))
       (setf *activitypub-private-key*
             (trivia:match (asn1:decode (base64:base64-string-to-usb8-array (read-pem-body priv-path)))
               ((asn1:rsa-private-key :modulus n :public-exponent e :private-exponent d
                                      :prime1 p :prime2 q)
                (ironclad:make-private-key :rsa :n n :e e :d d :p p :q q))))
       (setf *activitypub-public-key-pem* (alexandria:read-file-into-string pub-path))
       (setf *activitypub-public-key*
             (trivia:match (asn1:decode (base64:base64-string-to-usb8-array (read-pem-body pub-path)))
               ((asn1:rsa-public-key-info n e)
                (ironclad:make-public-key :rsa :n n :e e))))
       t)
      (generate-if-missing
       (multiple-value-bind (priv pub pem) (generate-ephemeral-rsa-keypair)
         (setf *activitypub-private-key* priv
               *activitypub-public-key* pub
               *activitypub-public-key-pem* pem))
       :ephemeral)
      (t
       (error "ActivityPub keys not found under ~A and :generate-if-missing is NIL." base)))))

;;; AWS setup — deferred to configure-aws!. Until it runs the application
;;; does not touch the network at all, so the system loads fine without
;;; AWS_ACCESS_KEY / AWS_SECRET_KEY.

(defparameter *aws-region* "eu-west-1")

(defclass environment-credentials () ())

(defmethod access-key ((credentials environment-credentials))
  (declare (ignore credentials))
  (asdf::getenv "AWS_ACCESS_KEY"))

(defmethod secret-key ((credentials environment-credentials))
  (declare (ignore credentials))
  (asdf::getenv "AWS_SECRET_KEY"))

(defmethod loaded ((credentials environment-credentials))
  (and (not (eq (access-key credentials) nil))
       (not (eq (secret-key credentials) nil))))

(defmethod print-object ((credentials environment-credentials) out)
  (print-unreadable-object (credentials out :type t)
    (if (loaded credentials)
	(format out "Environment credentials ~A:~A" (access-key credentials) (secret-key credentials))
	(format out "Empty credentials (envvars not set up)"))))

(defvar *credentials* nil "AWS credentials, populated by configure-aws!")

(defvar *dyna* nil "Connection to DynamoDB, populated by initialize-dyna!")

(defparameter *static-bucket* "rayslava-statics"
  "Name of AWS S3 bucket with static files")

(defun aws-credentials-available-p ()
  "Is AWS_ACCESS_KEY set in the environment?"
  (let ((k (asdf::getenv "AWS_ACCESS_KEY")))
    (and k (not (string= "" k)))))

(defun configure-aws! ()
  "Point zs3 at the configured region and install environment credentials.
Must be called before any S3 operations."
  (setf zs3:*s3-endpoint* (concatenate 'string "s3-" *aws-region* ".amazonaws.com"))
  (setf zs3:*s3-region* *aws-region*)
  (setf zs3:*use-ssl* t)
  (setf zs3:*signed-payload* nil)
  (unless *credentials*
    (setf *credentials* (make-instance 'environment-credentials))))

(defun initialize-dyna! ()
  "Create the DynamoDB client. Requires configure-aws! first if AWS mode."
  (unless *credentials*
    (setf *credentials* (make-instance 'environment-credentials)))
  (setf *dyna*
        (if (and *credentials* (loaded *credentials*))
            (dyna:make-dyna :credentials (cons (access-key *credentials*)
                                               (secret-key *credentials*))
                            :region *aws-region*)
            (dyna:make-dyna :region "local"))))
