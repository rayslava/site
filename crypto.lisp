(defpackage :site.crypto
  (:use :cl :hunchentoot :cl-who :cl-json :asdf)
  (:export :rsassa-pkcs1-v1_5-sign
   :rsassa-pkcs1-v1_5-verify))

(in-package :site.crypto)

;;; Thanks to dnaeon for this section
;;; https://dnaeon.github.io/rsassa-pkcs1-1_5-signature-common-lisp/

(defun i2osp (n &key n-bits)
  "Integer-to-Octet-String primitive. See RFC 8017, section 4.1"
  (declare (type integer n))
  (let ((n-bits (or n-bits (integer-length n))))
    (ironclad:integer-to-octets n :n-bits n-bits)))

(defun os2ip (octet-vec)
  "Octet-String-to-Integer primitive. See RFC 8017, section 4.2"
  (ironclad:octets-to-integer octet-vec))

;;;;
;;;; Signature and verification primitives - RFC 8017, section 5.2
;;;;

(defun rsasp1 (priv-key message)
  "RSA signature primitive. See RFC 8017, section 5.2.1"
  (declare (type integer message))
  (let ((n (ironclad:rsa-key-modulus priv-key))
        (d (ironclad:rsa-key-exponent priv-key)))
    (unless (<= 0 message (1- n))
      (error "message representative out of range"))
    (ironclad:expt-mod message d n)))

(defun rsavp1 (public-key signature)
  "RSA verification primitive. See RFC 8017, section 5.2.2"
  (declare (type integer signature))
  (let ((n (ironclad:rsa-key-modulus public-key))
        (e (ironclad:rsa-key-exponent public-key)))
    (unless (<= 0 signature (1- n))
      (error "signature representative out of range"))
    (ironclad:expt-mod signature e n)))

;;;;
;;;; Encoding Methods for Signatures with Appendix
;;;;

(defparameter *emsa-pkcs1-v1_5-digest-info*
  '(:md2    #(#x30 #x20 #x30 #x0c #x06 #x08 #x2a #x86 #x48 #x86 #xf7 #x0d #x02 #x02 #x05 #x00 #x04 #x10)
    :md5    #(#x30 #x20 #x30 #x0c #x06 #x08 #x2a #x86 #x48 #x86 #xf7 #x0d #x02 #x05 #x05 #x00 #x04 #x10)
    :sha1   #(#x30 #x21 #x30 #x09 #x06 #x05 #x2b #x0e #x03 #x02 #x1a #x05 #x00 #x04 #x14)
    :sha256 #(#x30 #x31 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x01 #x05 #x00 #x04 #x20)
    :sha384 #(#x30 #x41 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x02 #x05 #x00 #x04 #x30)
    :sha512 #(#x30 #x51 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x03 #x05 #x00 #x04 #x40))
  "DigestInfo DER encoding of the known hash functions. See RFC 8017, section 9.2, notes 1.")

(defun emsa-pkcs1-v1_5-encode (digest-spec message em-len)
  "EMSA-PKCS1-v1_5 encoding method. See RFC 8017, section 9.2"
  (unless (member digest-spec *emsa-pkcs1-v1_5-digest-info*)
    (error "unsupported digest spec"))
  (let* ((algorithm-identifier (getf *emsa-pkcs1-v1_5-digest-info* digest-spec))
         (h (ironclad:digest-sequence digest-spec message)) ;; Step 1
         (tt (concatenate '(vector (unsigned-byte 8) *)     ;; Step 2
                          algorithm-identifier
                          h))
         (tt-len (length tt)))
    (when (< em-len (+ tt-len 11)) ;; Step 3
      (error "intended encoded message length too short"))
    (let* ((ps (make-array (- em-len tt-len 3)  ;; Step 4
                           :element-type '(unsigned-byte 8)
                           :initial-element #xff)))
      (when (< (length ps) 8)
        (error "PS length should be at least 8 octets"))
      ;; Step 5 and 6
      (concatenate '(vector (unsigned-byte 8) *)
                   #(#x00 #x01) ps #(#x00) tt))))

;;;;
;;;; Signature Scheme with Appendix - RFC 8017, section 8
;;;;

(defun rsassa-pkcs1-v1_5-sign (priv-key message digest-spec)
  "RSASSA-PKCS1-v1_5 signature generation. See RFC 8017, section 8.2.1"
  (let* ((n (ironclad:rsa-key-modulus priv-key))
         (k (ceiling (integer-length n) 8))
         (em (emsa-pkcs1-v1_5-encode digest-spec message k))  ;; Step 1
         (m (os2ip em))  ;; Step 2a
         (s (rsasp1 priv-key m)))  ;; Step 2b
    ;; Step 2c and 3
    (i2osp s :n-bits (* 8 k))))

(defun rsassa-pkcs1-v1_5-verify (public-key message signature digest-spec)
  "RSASSA-PKCS1-v1_5 signature verification. See RFC 8017, section 8.2.2"
  (let* ((n (ironclad:rsa-key-modulus public-key))
         (k (ceiling (integer-length n) 8)))
    ;; Step 1
    (unless (= k (length signature))
      (error "invalid signature"))
    (let* ((s (os2ip signature))                                       ;; Step 2a
           (m (rsavp1 public-key s))                                   ;; Step 2b
           (em (i2osp m :n-bits (* 8 k)))                              ;; Step 2c
           (em-prime (emsa-pkcs1-v1_5-encode digest-spec message k)))  ;; Step 3
      ;; Step 4
      (equalp em em-prime))))
