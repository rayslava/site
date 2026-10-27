(in-package :site.tests)

(in-suite all-tests)

;;;; Inbox request verification: the pure checks in site.ap-signature,
;;;; and signed POSTs against a live server with the key fetch, DNS and
;;;; outgoing deliveries stubbed.

;;; Pure checks

(defun octets (string)
  (flex:string-to-octets string :external-format :utf-8))

(defun sha256-digest-header (body)
  (concatenate 'string "SHA-256="
               (base64:usb8-array-to-base64-string
                (ironclad:digest-sequence :sha256 (octets body)))))

(test inbox-digest-matches-body
  (let ((body "{\"type\":\"Follow\"}"))
    (is-true (site.ap-signature:digest-matches-p (sha256-digest-header body) (octets body)))
    (is-true (site.ap-signature:digest-matches-p
              (concatenate 'string "sha-256=" (subseq (sha256-digest-header body) 8))
              (octets body)))
    (is-false (site.ap-signature:digest-matches-p (sha256-digest-header body) (octets "{}")))
    (is-false (site.ap-signature:digest-matches-p nil (octets body)))
    (is-false (site.ap-signature:digest-matches-p "SHA-512=abc" (octets body)))))

(test inbox-parse-http-date
  (is (= (encode-universal-time 35 51 20 7 6 2022 0)
         (site.ap-signature:parse-http-date "Tue, 07 Jun 2022 20:51:35 GMT")))
  (is (null (site.ap-signature:parse-http-date "2022-06-07T20:51:35Z")))
  (is (null (site.ap-signature:parse-http-date nil))))

(test inbox-date-fresh
  (let ((now (encode-universal-time 0 0 12 1 1 2026 0)))
    (flet ((fresh (offset)
             (site.ap-signature:date-fresh-p
              (hunchentoot:rfc-1123-date (+ now offset)) :now now)))
      (is-true (fresh 0))
      (is-true (fresh (- (* 11 3600))))
      (is-true (fresh 1800))
      (is-false (fresh (- (* 13 3600))))
      (is-false (fresh 7200))
      (is-false (site.ap-signature:date-fresh-p nil :now now)))))

(test inbox-signature-covers-required-headers
  (is-true (site.ap-signature:signature-covers-required-p
            '("(request-target)" "host" "date" "digest" "content-type")))
  (is-false (site.ap-signature:signature-covers-required-p
             '("(request-target)" "host" "date")))
  (is-false (site.ap-signature:signature-covers-required-p nil)))

(defun fake-resolver (table)
  "Resolver returning the addresses listed for a host in TABLE, an alist
of host to dotted-quad strings."
  (lambda (host)
    (mapcar (lambda (quad)
              (coerce (mapcar #'parse-integer (cl-ppcre:split "\\." quad)) 'vector))
            (cdr (assoc host table :test #'string-equal)))))

(test inbox-public-https-url
  (let ((resolve (fake-resolver '(("social.example" "93.184.216.34")
                                  ("rebind.example" "93.184.216.34" "10.1.2.3")
                                  ("metadata.example" "169.254.169.254")
                                  ("10.0.0.1" "10.0.0.1")))))
    (flet ((public (url) (site.ap-signature:public-https-url-p url :resolve resolve)))
      (is-true (public "https://social.example/users/alice#main-key"))
      (is-false (public "http://social.example/users/alice#main-key"))
      (is-false (public "https://site:8080/admin"))
      (is-false (public "https://rss-bridge/"))
      (is-false (public "https://10.0.0.1/key"))
      (is-false (public "https://[::1]/key"))
      (is-false (public "https://metadata.example/latest/meta-data"))
      (is-false (public "https://rebind.example/key"))
      (is-false (public "https://unresolvable.example/key"))
      (is-false (public nil)))))

(test inbox-signer-matches-actor
  (let ((doc '((:id . "https://social.example/users/alice")
               (:public-key . ((:id . "https://social.example/users/alice#main-key")
                               (:owner . "https://social.example/users/alice"))))))
    (is-true (site.ap-signature:signer-matches-actor-p
              "https://social.example/users/alice#main-key" doc
              "https://social.example/users/alice"))
    (is-false (site.ap-signature:signer-matches-actor-p
               "https://social.example/users/alice#main-key" doc
               "https://social.example/users/bob"))
    (is-false (site.ap-signature:signer-matches-actor-p
               "https://evil.example/users/alice#main-key" doc
               "https://social.example/users/alice"))
    (is-false (site.ap-signature:signer-matches-actor-p
               "https://social.example/users/alice#main-key" doc nil))))

(test inbox-undo-follow-only-unsubscribes-sender
  "An Undo Follow naming another actor inside must not unsubscribe them."
  (with-quiet-http-and-memory
    (site.storage:subscriber-save "https://social.example/users/bob"
                                  '(("subscribed" . "x")))
    (site.activitypub:handle-inbox-activity
     '((:type . "Undo")
       (:actor . "https://evil.example/users/mallory")
       (:object . ((:type . "Follow")
                   (:actor . "https://social.example/users/bob"))))
     "{}")
    (is-true (site.storage:subscriber-find "https://social.example/users/bob"))))

(test config-credentials-print-hides-secrets
  (let ((saved-access (uiop:getenv "AWS_ACCESS_KEY"))
        (saved-secret (uiop:getenv "AWS_SECRET_KEY")))
    (unwind-protect
         (progn
           (setf (uiop:getenv "AWS_ACCESS_KEY") "AKIATESTACCESS"
                 (uiop:getenv "AWS_SECRET_KEY") "test-secret-value")
           (let ((printed (prin1-to-string
                           (make-instance 'site.config:environment-credentials))))
             (is (null (search "AKIATESTACCESS" printed)))
             (is (null (search "test-secret-value" printed)))))
      (setf (uiop:getenv "AWS_ACCESS_KEY") (or saved-access "")
            (uiop:getenv "AWS_SECRET_KEY") (or saved-secret "")))))

;;; Signed requests against a live server

(defun der (tag content)
  "DER encoding of CONTENT (a list of octets) under TAG."
  (let ((n (length content)))
    (append (list tag)
            (if (< n 128)
                (list n)
                (let ((len (loop for x = n then (ash x -8) while (> x 0)
                                 collect (logand x 255))))
                  (cons (logior 128 (length len)) (reverse len))))
            content)))

(defun der-integer (n)
  (let ((bytes (reverse (loop for x = n then (ash x -8) while (> x 0)
                              collect (logand x 255)))))
    (der #x02 (if (>= (first bytes) 128) (cons 0 bytes) bytes))))

(defun public-key-pem (public-key)
  "PEM SubjectPublicKeyInfo for an ironclad RSA PUBLIC-KEY, as actors
serve it in publicKeyPem."
  (let* ((rsa (der #x30 (append (der-integer (ironclad:rsa-key-modulus public-key))
                                (der-integer (ironclad:rsa-key-exponent public-key)))))
         (spki (der #x30 (append (der #x30 '(#x06 #x09 #x2a #x86 #x48 #x86 #xf7 #x0d
                                             #x01 #x01 #x01 #x05 #x00))
                                 (der #x03 (cons 0 rsa)))))
         (b64 (base64:usb8-array-to-base64-string
               (coerce spki '(vector (unsigned-byte 8))))))
    (with-output-to-string (s)
      (format s "-----BEGIN PUBLIC KEY-----~%")
      (loop for i from 0 below (length b64) by 64
            do (format s "~A~%" (subseq b64 i (min (length b64) (+ i 64)))))
      (format s "-----END PUBLIC KEY-----~%"))))

(defvar *signer-private-key* nil)
(defvar *signer-pem* nil)
(defvar *key-fetches* nil "keyIds the stubbed key fetch was asked for.")

(defparameter *alice* "https://social.example/users/alice")
(defparameter *alice-key* "https://social.example/users/alice#main-key")

(defun ensure-signer-key ()
  (unless *signer-private-key*
    (multiple-value-bind (priv pub) (ironclad:generate-key-pair :rsa :num-bits 2048)
      (setf *signer-private-key* priv
            *signer-pem* (public-key-pem pub)))))

(defun signer-document (keyid)
  "Actor document served for any keyId: alice, owning the signer key."
  (push keyid *key-fetches*)
  (cl-json:encode-json-to-string
   `((:id . ,*alice*)
     (:type . "Person")
     (:public-key . ((:id . ,*alice-key*)
                     (:owner . ,*alice*)
                     (:public-key-pem . ,*signer-pem*))))))

(defmacro with-signed-inbox-server (&body body)
  "Live server with the key fetch answering with alice's document, every
dotted host resolving to a public address, and deliveries captured."
  `(let ((saved-fetch site.activitypub:*fetch-key-fn*)
         (saved-resolve site.ap-signature:*resolve-fn*)
         (saved-send site.activitypub:*http-send-fn*))
     (ensure-signer-key)
     (setf *key-fetches* nil
           site.activitypub:*fetch-key-fn* #'signer-document
           site.ap-signature:*resolve-fn*
           (lambda (host)
             (if (or (string-equal host "localhost")
                     (cl-ppcre:scan "^10\\." host))
                 (list (vector 10 0 0 1))
                 (list (vector 93 184 216 34))))
           site.activitypub:*http-send-fn*
           (lambda (url &key headers content)
             (declare (ignore url headers content))
             "{}"))
     (unwind-protect
          (with-integration-server ,@body)
       (setf site.activitypub:*fetch-key-fn* saved-fetch
             site.ap-signature:*resolve-fn* saved-resolve
             site.activitypub:*http-send-fn* saved-send))))

(defun signed-inbox-post (body &key (keyid *alice-key*)
                                    (date (hunchentoot:rfc-1123-date))
                                    (digest (sha256-digest-header body))
                                    (covered "(request-target) host date digest")
                                    (sent-body body))
  "POST SENT-BODY to the inbox, signed as KEYID over BODY's digest.
Returns the status code."
  (let* ((host (format nil "localhost:~A" *test-port*))
         (values `((:host . ,host) (:date . ,date) (:digest . ,digest)))
         (message (site.ap-signature:build-signed-message
                   "post" "/ap/actor/blog/inbox" values
                   (cl-ppcre:split " " covered)))
         (signature (base64:usb8-array-to-base64-string
                     (site.crypto:rsassa-pkcs1-v1_5-sign
                      *signer-private-key*
                      (ironclad:ascii-string-to-byte-array message)
                      :sha256))))
    (handler-case
        (nth-value 1
                   (dex:post (url "/ap/actor/blog/inbox")
                             :headers `(("content-type" . "application/activity+json")
                                        ("date" . ,date)
                                        ("digest" . ,digest)
                                        ("signature" .
                                         ,(format nil "keyId=\"~A\",algorithm=\"rsa-sha256\",headers=\"~A\",signature=\"~A\""
                                                  keyid covered signature)))
                             :content sent-body
                             :verbose nil))
      (dex:http-request-failed (e) (dex:response-status e)))))

(defun follow-json (actor)
  (cl-json:encode-json-to-string
   `((:type . "Follow")
     (:id . ,(concatenate 'string actor "#follow/1"))
     (:actor . ,actor)
     (:object . "https://rayslava.com/ap/actor/blog"))))

(test int-inbox-accepts-signed-follow
  (with-signed-inbox-server
    (is (= 200 (signed-inbox-post (follow-json *alice*))))
    (is-true (site.storage:subscriber-find *alice*))
    (is (equal (list *alice-key*) *key-fetches*))))

(test int-inbox-rejects-follow-for-another-actor
  "A valid signature from alice cannot subscribe somebody else."
  (with-signed-inbox-server
    (is (= 401 (signed-inbox-post (follow-json "https://social.example/users/bob"))))
    (is (= 401 (signed-inbox-post (follow-json "https://victim.example/users/alice"))))
    (is (null (site.storage:subscriber-find "https://social.example/users/bob")))
    (is (null (site.storage:subscriber-find "https://victim.example/users/alice")))))

(test int-inbox-rejects-body-not-matching-digest
  (with-signed-inbox-server
    (is (= 401 (signed-inbox-post (follow-json *alice*)
                                  :sent-body (follow-json "https://social.example/users/bob"))))
    (is (null *key-fetches*))))

(test int-inbox-rejects-stale-date
  (with-signed-inbox-server
    (is (= 401 (signed-inbox-post (follow-json *alice*)
                                  :date (hunchentoot:rfc-1123-date
                                         (- (get-universal-time) (* 13 3600))))))
    (is (null (site.storage:subscriber-find *alice*)))))

(test int-inbox-rejects-signature-without-digest
  (with-signed-inbox-server
    (is (= 401 (signed-inbox-post (follow-json *alice*)
                                  :covered "(request-target) host date")))))

(test int-inbox-rejects-missing-signature-with-401
  (with-signed-inbox-server
    (is (= 401 (nth-value 1 (int-post "/ap/actor/blog/inbox" (follow-json *alice*)
                                      :content-type "application/activity+json"))))))

(test int-inbox-does-not-fetch-internal-key-urls
  (with-signed-inbox-server
    (dolist (keyid '("http://site:8080/admin" "https://rss-bridge/" "https://localhost/key"
                     "https://10.0.0.1/key" "http://social.example/users/alice#main-key"))
      (is (= 401 (signed-inbox-post (follow-json *alice*) :keyid keyid))))
    (is (null *key-fetches*))))
