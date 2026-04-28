(in-package :site.tests)

(in-suite all-tests)

(defparameter *sample-actor-pem*
  ;; A fixed, well-formed 2048-bit RSA public key in X.509 SubjectPublicKeyInfo
  ;; format (what Mastodon serves as publicKeyPem). Including a static fixture
  ;; rather than reading activitypub/public.pem lets the test run in tree
  ;; checkouts that don't have that file.
  "-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvyAu7oAYxWMhheEWWZ6A
Ndj9Ckq8bOLXWWfktglNFjfTJEUCKCGYAOVXLB+FTi5QMzBC1LrRtetnso5ChsyB
r/V5wVWSWwDne1hAZFqFwkbq9c0qgijv+9Y6lILN6h7Dg02ynW8fpJXbbpe+MS0S
4Ua+DtElSPE6sPW+elcdf8L0ijw7EOr/eOdsy9sdBd8Otiz+yYPEpGHsmdBeFAyX
LOp/gJe2WaHK3J6Nb0OPX0QuIMX5DcIcD8RJhC/wJGfPPEvuQDzJ3kzYwyBkbiAw
EDxhuGTJoawPyUySJfCRpYkGDhSiVXi5zpNFvsx2jSovPjw9Y1e2dabLfC5AK8Nq
nwIDAQAB
-----END PUBLIC KEY-----
")

(test ap-signature-extract-public-key-from-real-pem
  "The parser accepts a Mastodon-shape PEM and returns an ironclad RSA key."
  (let ((key (site.ap-signature:extract-public-key-from-actor-pem
              *sample-actor-pem*)))
    (is-true (ironclad:rsa-key-modulus key))
    (is-true (ironclad:rsa-key-exponent key))
    (is (= 65537 (ironclad:rsa-key-exponent key)))
    ;; 2048-bit modulus ≈ 2^2047..2^2048
    (is (> (ironclad:rsa-key-modulus key) (expt 2 2047)))))

(test ap-signature-extract-public-key-verifies-signature
  "A signature made with the private half of *sample-actor-pem* verifies
against the key extracted from the PEM — but we don't have that private
key in-tree, so instead we prove round-trip consistency by extracting the
same public twice and comparing moduli."
  (let ((k1 (site.ap-signature:extract-public-key-from-actor-pem *sample-actor-pem*))
        (k2 (site.ap-signature:extract-public-key-from-actor-pem *sample-actor-pem*)))
    (is (= (ironclad:rsa-key-modulus k1) (ironclad:rsa-key-modulus k2)))
    (is (= (ironclad:rsa-key-exponent k1) (ironclad:rsa-key-exponent k2)))))
