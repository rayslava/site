(in-package :site.tests)

(in-suite all-tests)

(test ap-signature-parse-header
  "parse-signature-header extracts keyId, headers, and base64 signature."
  (let* ((raw "keyId=\"https://alice.example/users/alice#main-key\",algorithm=\"rsa-sha256\",headers=\"(request-target) host date digest\",signature=\"abcdef==\"")
         (sig (site.ap-signature:parse-signature-header raw)))
    (is (equal "https://alice.example/users/alice#main-key"
               (site.ap-signature:signature-keyid sig)))
    (is (equal "(request-target) host date digest"
               (site.ap-signature:signature-headers sig)))))

(test ap-signature-build-signed-message-layout
  "build-signed-message produces the exact byte layout the verifier expects."
  (let* ((headers '((:host . "rayslava.com")
                    (:date . "Tue, 07 Jun 2022 20:51:35 GMT")
                    (:digest . "SHA-256=abc")))
         (covered '("(request-target)" "host" "date" "digest"))
         (msg (site.ap-signature:build-signed-message
               "post" "/ap/actor/blog/inbox" headers covered)))
    (is (equal (format nil "(request-target): post /ap/actor/blog/inbox~%host: rayslava.com~%date: Tue, 07 Jun 2022 20:51:35 GMT~%digest: SHA-256=abc")
               msg))))

(test ap-signature-roundtrip-with-local-key
  "Sign a canonical message with a local RSA key and verify the round-trip
through build-signed-message + rsassa-pkcs1-v1_5-verify."
  (multiple-value-bind (priv pub) (generate-rsa-keypair)
    (let* ((headers '((:host . "example.com")
                      (:date . "Wed, 08 Jun 2022 01:00:00 GMT")))
           (covered '("(request-target)" "host" "date"))
           (msg (site.ap-signature:build-signed-message
                 "post" "/inbox" headers covered))
           (signature (site.crypto:rsassa-pkcs1-v1_5-sign
                       priv
                       (ironclad:ascii-string-to-byte-array msg)
                       :sha256)))
      (is-true (site.crypto:rsassa-pkcs1-v1_5-verify
                pub
                (ironclad:ascii-string-to-byte-array msg)
                signature
                :sha256)))))
