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
   :extract-public-key-from-actor-pem
   :signature-covers-required-p
   :digest-matches-p
   :parse-http-date
   :date-fresh-p
   :public-https-url-p
   :*resolve-fn*
   :same-host-p
   :signer-matches-actor-p))

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

(defparameter *required-signed-headers* '("(request-target)" "host" "date" "digest")
  "Headers an inbox signature must cover: without date and digest a
signature could be replayed or reused for another body.")

(defun signature-covers-required-p (covered-header-names)
  "True when the signer's `headers` list includes every required header."
  (every (lambda (name) (member name covered-header-names :test #'string-equal))
         *required-signed-headers*))

(defun digest-matches-p (digest-header body)
  "True when DIGEST-HEADER (\"SHA-256=<base64>\", possibly among other
comma-separated algorithms) carries the SHA-256 of the octet vector BODY."
  (let ((expected (base64:usb8-array-to-base64-string
                   (ironclad:digest-sequence :sha256 body))))
    (and digest-header
         (some (lambda (part)
                 (let ((sep (position #\= part)))
                   (and sep
                        (string-equal "SHA-256" (string-trim " " (subseq part 0 sep)))
                        (string= expected (string-trim " " (subseq part (1+ sep)))))))
               (cl-ppcre:split "," digest-header)))))

(defun parse-http-date (string)
  "Universal time of an HTTP date like \"Tue, 07 Jun 2022 20:51:35 GMT\",
or NIL when STRING is not one."
  (cl-ppcre:register-groups-bind ((#'parse-integer day) month
                                  (#'parse-integer year hour minute second))
      ("^\\w{3}, (\\d{1,2}) (\\w{3}) (\\d{4}) (\\d{2}):(\\d{2}):(\\d{2}) GMT$"
       (or string ""))
    (let ((m (position month '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                               "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                       :test #'string-equal)))
      (and m (ignore-errors
              (encode-universal-time second minute hour day (1+ m) year 0))))))

(defun date-fresh-p (date-header &key (now (get-universal-time))
                                      (max-age (* 12 3600)) (max-future 3600))
  "True when DATE-HEADER is at most MAX-AGE seconds old and at most
MAX-FUTURE seconds ahead of NOW. The limits are Mastodon's, so retried
deliveries from peers are still accepted."
  (let ((time (parse-http-date date-header)))
    (and time (<= (- now max-age) time (+ now max-future)))))

(defun private-ipv4-p (address)
  "True for loopback, private, link-local, CGNAT and unspecified addresses."
  (let ((a (aref address 0)) (b (aref address 1)))
    (or (= a 0) (= a 10) (= a 127)
        (and (= a 100) (<= 64 b 127))
        (and (= a 169) (= b 254))
        (and (= a 172) (<= 16 b 31))
        (and (= a 192) (= b 168)))))

(defun resolve-ipv4 (host)
  "IPv4 addresses of HOST as octet vectors, NIL when it does not resolve."
  (ignore-errors
   (sb-bsd-sockets:host-ent-addresses (sb-bsd-sockets:get-host-by-name host))))

(defvar *resolve-fn* 'resolve-ipv4
  "Function of a host name returning its IPv4 addresses; tests replace it.")

(defun public-https-url-p (url &key (resolve *resolve-fn*))
  "True when URL is https on a dotted host name whose addresses are all
public. Keeps key fetches away from the compose services (single-label
names), loopback and the cloud metadata address. Only IPv4 is checked:
SBCL resolves IPv4 only, so the HTTP client cannot reach IPv6-only hosts
either."
  (let* ((uri (and (stringp url) (ignore-errors (quri:uri url))))
         (host (and uri (quri:uri-host uri))))
    (and host
         (equal "https" (quri:uri-scheme uri))
         (find #\. host)
         (not (find #\: host))
         (let ((addresses (funcall resolve host)))
           (and addresses (notany #'private-ipv4-p addresses))))))

(defun same-host-p (url-a url-b)
  "True when both URLs parse and name the same host."
  (let ((a (and (stringp url-a) (ignore-errors (quri:uri-host (quri:uri url-a)))))
        (b (and (stringp url-b) (ignore-errors (quri:uri-host (quri:uri url-b))))))
    (and a b (string-equal a b))))

(defun signer-matches-actor-p (keyid key-document actor)
  "True when the key behind KEYID belongs to ACTOR: same host, and the
fetched KEY-DOCUMENT names ACTOR as the key's owner (or is ACTOR's own
document). Without this any fediverse key could act for any actor."
  (let* ((public-key (cdr (assoc :public-key key-document)))
         (owner (or (cdr (assoc :owner public-key))
                    (cdr (assoc :id key-document)))))
    (and (stringp actor)
         (same-host-p keyid actor)
         (equal owner actor))))

;; verify-inbox-request is intentionally NOT extracted: the real handler
;; uses handler-bind around the key fetch to short-circuit on HTTP 410/404,
;; preserving the "Delete from a gone actor still gets OK" behavior.
;; Keeping that flow in the handler means this module stays pure and test-
;; able. The handler composes parse-signature-header, build-signed-message,
;; extract-public-key-from-actor-pem, and rsassa-pkcs1-v1_5-verify itself.
