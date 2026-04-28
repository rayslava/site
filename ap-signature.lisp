;;; HTTP Signature verification for ActivityPub inbox requests.
;;;
;;; Extracted from activitypub.lisp so the parsing and message-building
;;; steps are pure functions, testable without a live HTTP server or a
;;; remote actor to fetch. The one impure step — fetching the signer's
;;; public key — is injected as a callback.

(defpackage :site.ap-signature
  (:use :cl :site.crypto)
  (:export
   :parse-signature-header
   :signature-keyid
   :signature-headers
   :signature-bytes
   :build-signed-message
   :extract-public-key-from-actor-pem))

(in-package :site.ap-signature)

(defstruct parsed-signature
  keyid
  algorithm
  headers
  signature-base64)

(defun signature-keyid (p) (parsed-signature-keyid p))
(defun signature-headers (p) (parsed-signature-headers p))
(defun signature-bytes (p)
  (base64:base64-string-to-usb8-array (parsed-signature-signature-base64 p)))

(defun %trim-quotes (s)
  (string-trim "\"" s))

(defun parse-signature-header (raw)
  "Parse an HTTP Signature header value (RFC-ish Mastodon flavor) into a
PARSED-SIGNATURE. Input is a string like
  keyId=\"...\",algorithm=\"rsa-sha256\",headers=\"(request-target) host date digest\",signature=\"...\"
Fields appear in arbitrary order; values are quoted."
  (let* ((pairs (cl-ppcre:split "," raw))
         (parts (reduce 'nconc
                        (mapcar (lambda (l)
                                  (coerce
                                   (nth-value 1
                                              (cl-ppcre:scan-to-strings "(\\w*)=(.*\\\")$" l))
                                   'list))
                                pairs)))
         (alist (loop for (head . tail) on parts by #'cddr
                      collect (cons head (car tail)))))
    (flet ((g (name) (%trim-quotes (cdr (assoc name alist :test #'string-equal)))))
      (make-parsed-signature
       :keyid            (g "keyid")
       :algorithm        (g "algorithm")
       :headers          (g "headers")
       :signature-base64 (g "signature")))))

(defun build-signed-message (method path headers-in covered-header-names)
  "Reconstruct the canonical byte string the signer signed.

METHOD is a string like \"post\". PATH is the request URI path.
HEADERS-IN is an alist of keyword → string from HUNCHENTOOT:HEADERS-IN.
COVERED-HEADER-NAMES is the list from the signer's `headers` field, split
on spaces. The first entry is expected to be `(request-target)`; that
pseudo-header becomes the prefix, and each subsequent name is emitted
on its own line with its value from HEADERS-IN.

The case and spacing of header names is preserved as received, because
that is what the signer hashed."
  (let ((prefix (format nil "(request-target): ~A ~A" method path))
        (rest (with-output-to-string (s)
                (dolist (hdr (rest covered-header-names))
                  (format s "~%~A: ~A"
                          hdr
                          (cdr (assoc (intern (string-upcase hdr) :keyword)
                                      headers-in)))))))
    (concatenate 'string prefix rest)))

(defun extract-public-key-from-actor-pem (pem)
  "Given a PEM-formatted publicKeyPem string from an ActivityPub actor,
return an ironclad :RSA public-key object."
  (trivia:match
      (asn1:decode
       (base64:base64-string-to-usb8-array
        (cl-ppcre:regex-replace-all "\\n"
                                    (cl-ppcre:regex-replace-all ".*----\\n" pem "")
                                    "")))
    ((asn1:rsa-public-key-info n e)
     (ironclad:make-public-key :rsa :n n :e e))))

;; verify-inbox-request is intentionally NOT extracted: the real handler
;; uses handler-bind around the key fetch to short-circuit on HTTP 410/404,
;; preserving the "Delete from a gone actor still gets OK" behavior.
;; Keeping that flow in the handler means this module stays pure and test-
;; able. The handler composes parse-signature-header, build-signed-message,
;; extract-public-key-from-actor-pem, and rsassa-pkcs1-v1_5-verify itself.
