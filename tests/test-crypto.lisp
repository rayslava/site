(in-package :site.tests)

(in-suite all-tests)

(defun generate-rsa-keypair (&key (n-bits 2048))
  "Fresh in-memory keypair; no disk, no network."
  (multiple-value-bind (priv pub)
      (ironclad:generate-key-pair :rsa :num-bits n-bits)
    (values priv pub)))

(test crypto-sign-verify-roundtrip
  "rsassa-pkcs1-v1_5 sign then verify must succeed for the original message."
  (multiple-value-bind (priv pub) (generate-rsa-keypair)
    (let* ((msg (ironclad:ascii-string-to-byte-array
                 "The quick brown fox jumps over the lazy dog"))
           (sig (site.crypto:rsassa-pkcs1-v1_5-sign priv msg :sha256)))
      (is-true (site.crypto:rsassa-pkcs1-v1_5-verify pub msg sig :sha256)))))

(test crypto-verify-rejects-tampered-message
  "Verification must fail if the message bytes change after signing."
  (multiple-value-bind (priv pub) (generate-rsa-keypair)
    (let* ((msg (ironclad:ascii-string-to-byte-array "original payload"))
           (tampered (ironclad:ascii-string-to-byte-array "tampered payload"))
           (sig (site.crypto:rsassa-pkcs1-v1_5-sign priv msg :sha256)))
      (is-false (site.crypto:rsassa-pkcs1-v1_5-verify pub tampered sig :sha256)))))

(test crypto-verify-rejects-wrong-key
  "Signature from key A must not verify against public key B."
  (multiple-value-bind (priv-a pub-a) (generate-rsa-keypair)
    (declare (ignore pub-a))
    (multiple-value-bind (priv-b pub-b) (generate-rsa-keypair)
      (declare (ignore priv-b))
      (let* ((msg (ironclad:ascii-string-to-byte-array "cross-key check"))
             (sig (site.crypto:rsassa-pkcs1-v1_5-sign priv-a msg :sha256)))
        (is-false (site.crypto:rsassa-pkcs1-v1_5-verify pub-b msg sig :sha256))))))
