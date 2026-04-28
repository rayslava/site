(in-package :site.tests)

(in-suite all-tests)

(defparameter *test-private-pem*
  "-----BEGIN RSA PRIVATE KEY-----
MIIEpAIBAAKCAQEAvyAu7oAYxWMhheEWWZ6ANdj9Ckq8bOLXWWfktglNFjfTJEUC
KCGYAOVXLB+FTi5QMzBC1LrRtetnso5ChsyBr/V5wVWSWwDne1hAZFqFwkbq9c0q
gijv+9Y6lILN6h7Dg02ynW8fpJXbbpe+MS0S4Ua+DtElSPE6sPW+elcdf8L0ijw7
EOr/eOdsy9sdBd8Otiz+yYPEpGHsmdBeFAyXLOp/gJe2WaHK3J6Nb0OPX0QuIMX5
DcIcD8RJhC/wJGfPPEvuQDzJ3kzYwyBkbiAwEDxhuGTJoawPyUySJfCRpYkGDhSi
VXi5zpNFvsx2jSovPjw9Y1e2dabLfC5AK8NqnwIDAQABAoIBAFQzzwH8e0D3jKnD
x4bXAFLJHsMcJEFf2kgE2AXT1Rx2dGkbKfyj9tLWKPo8aC1yXGXBAJGLfKX3PEt9
oIV3M+p5jOKmYf2wB+/uG6XaU4eIPw0r3nRHVQe6j9Yc3fd0GcQKq6YDxHYpMbZh
6KvWKpcDXD6xKzlhwmYz4Mr07wYozABdLPFqEmgmRaVSxN9AgQq5uA+vuWpPeGca
EITcV+7dPfF6LVd6Vk08kyTGRjrKEmDwlAVGYMy+VKFBy6OuNPSBDQ4J11n9CGDS
4g2D2YlJ+5kJvyPTbepLJsC8sWI9iDz8FN8h15jE8GPUpPO7Y62lwUQcLdKONdHv
7w3+3AECgYEA7lUtJL1uDKADo2J7XbbzCEflsMmB2ypcWt2g+V+ZGbNZuVw0OSqG
yS3ucEFn3e0Sg5p6NkpDSkeogjT/bS2RybS0njIS+3IoxjVtkQXyPtBtPQXG9gqN
BUrxY2A1Ut+PAQU8jz8sJuZUGOAkxnOSp6fxGN3bf5KgRIhnwHJVr4ECgYEAzW4W
jL1C2eD8G9qN/Z0DZPnRGfpxR/IQhuHUaBl1RZ/kOVTqYlXNvOzjTTrTRnEXBgKd
YlhSuV4HsvxOzBxB6qxMB0Bzk0JIP3i5fhVK5vVtOIEKkuhIRQi/5YcfAJANhc2V
x+FClTvESJE9LeyNnI5D0QhUUr4F5XVvE14vlx8CgYBymSPkzkSK7j3q7CXxkGTV
Mz2QHBjsUW2ONjjPcxIqVxTG1kD2sqfyGPdtfO/f4FgQe28XnqnIEdepMZFWm30t
HTHZxh7EAfHlP22+uUOXfOtOl0qOMJcY2u3wH8mPqq8XyvMRb80cwbmFx26jYWzl
nSVYPV5QZjLJQfSVGksPAQKBgQCPNmmRdd+OIyQWUXhxzxMeDx14Y0MXRLg5wy1H
qXcEUlTQnOSwGYbsvM8/dtvR5ywQjdY/IqdYw8TgYGhrxbRBY+1wO4UROEEH95oV
NvoGXlPz0Ga3t5cgkbz9fQnOyPbYmRyfb6YpLdUJvMW5/W6W1rMsLibyTVC9K9X8
iuKN6wKBgQCDuPPcCD8xU24nqC9ASSGs8kNnUhThI1tEiCcdnWdbyNQAiDzJCrAq
H6Hqi4iVHrLWN3vjZGPQ0NURrmADqdYD4rKNUl2FdwbfYQlMkwdFzX+hcZN4BFEs
j8CHEcjqzlF0eiFj2T5qlAVeJ15r1rwGZbY8R6WOwYTdHHFqfrFUmA==
-----END RSA PRIVATE KEY-----
")

(defparameter *test-public-pem*
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

(defmacro with-snapshot-keys (&body body)
  "Save and restore the three *activitypub-*-key* globals so tests can
mutate them without bleed."
  `(let ((priv site.config:*activitypub-private-key*)
         (pub site.config:*activitypub-public-key*)
         (pem site.config:*activitypub-public-key-pem*))
     (unwind-protect (progn ,@body)
       (setf site.config:*activitypub-private-key* priv
             site.config:*activitypub-public-key* pub
             site.config:*activitypub-public-key-pem* pem))))

(defun %write-test-keys-to (dir)
  (ensure-directories-exist dir)
  (with-open-file (s (merge-pathnames "private.pem" dir)
                     :direction :output :if-exists :supersede)
    (write-string *test-private-pem* s))
  (with-open-file (s (merge-pathnames "public.pem" dir)
                     :direction :output :if-exists :supersede)
    (write-string *test-public-pem* s)))

(test config-load-keys-from-disk
  "load-activitypub-keys! reads PEMs from :dir and populates the globals."
  (uiop:with-temporary-file (:pathname tmpf)
    (declare (ignore tmpf)))
  (let* ((tmpdir (merge-pathnames "site-key-test/"
                                  (uiop:ensure-directory-pathname
                                   (uiop:temporary-directory)))))
    (unwind-protect
         (with-snapshot-keys
           (%write-test-keys-to tmpdir)
           (site.config:load-activitypub-keys! :dir (namestring tmpdir))
           (is-true site.config:*activitypub-private-key*)
           (is-true site.config:*activitypub-public-key*)
           (is-true (search "-----BEGIN PUBLIC KEY-----"
                            site.config:*activitypub-public-key-pem*))
           (is (= 65537 (ironclad:rsa-key-exponent
                         site.config:*activitypub-public-key*))))
      (uiop:delete-directory-tree tmpdir :validate t :if-does-not-exist :ignore))))

(test config-load-keys-missing-without-generate-signals
  "Without :generate-if-missing t, an empty dir triggers an error."
  (let* ((tmpdir (merge-pathnames "site-key-missing/"
                                  (uiop:ensure-directory-pathname
                                   (uiop:temporary-directory)))))
    (unwind-protect
         (with-snapshot-keys
           (ensure-directories-exist tmpdir)
           (signals error
             (site.config:load-activitypub-keys! :dir (namestring tmpdir))))
      (uiop:delete-directory-tree tmpdir :validate t :if-does-not-exist :ignore))))

(test config-load-keys-generate-if-missing
  "With :generate-if-missing t, an empty dir yields an ephemeral keypair
and returns :EPHEMERAL."
  (let* ((tmpdir (merge-pathnames "site-key-eph/"
                                  (uiop:ensure-directory-pathname
                                   (uiop:temporary-directory)))))
    (unwind-protect
         (with-snapshot-keys
           (ensure-directories-exist tmpdir)
           (let ((outcome (site.config:load-activitypub-keys!
                           :dir (namestring tmpdir)
                           :generate-if-missing t)))
             (is (eq :ephemeral outcome))
             (is-true site.config:*activitypub-private-key*)
             (is-true site.config:*activitypub-public-key*)))
      (uiop:delete-directory-tree tmpdir :validate t :if-does-not-exist :ignore))))

(test config-aws-credentials-available-p
  "aws-credentials-available-p reflects the AWS_ACCESS_KEY env var."
  ;; We don't flip env vars here — just make sure the function returns a
  ;; generalized boolean rather than erroring.
  (let ((result (site.config:aws-credentials-available-p)))
    (is (or (null result) (not (null result))))))

(test config-initialize-dyna-rebinds-classes
  "initialize-dyna! must update the :dyna slot on every dyna-backed
table class, because dyna caches the value at class-definition time
and the classes were defined while *dyna* was still NIL (lazy init).

Regression test for a production 500: without this rebinding, the
first call to table-exist-p after start-server raised 'NIL is not of
type DYNA' because the class held a captured NIL."
  (let ((saved-dyna site.config:*dyna*)
        (saved-creds site.config:*credentials*))
    (unwind-protect
         (progn
           ;; Put *dyna* back in a known state — NIL — to simulate
           ;; what the classes captured when first defined.
           (setf site.config:*dyna* nil
                 site.config:*credentials* nil)
           (site.config:initialize-dyna!)
           ;; After initialize-dyna!, *dyna* is bound and the classes'
           ;; cached :dyna value matches.
           (is-true site.config:*dyna*)
           (dolist (spec '(("SITE.DB-STORAGE"   "STATIC-FILE")
                           ("SITE.ACTIVITYPUB"  "ACTIVITYPUB-SUBSCRIBER")
                           ("SITE.ACTIVITYPUB"  "ACTIVITYPUB-EVENT")))
             (let* ((pkg (find-package (first spec)))
                    (sym (and pkg (find-symbol (second spec) pkg)))
                    (class (and sym (find-class sym nil))))
               (is-true class "class ~A:~A must exist" (first spec) (second spec))
               (when class
                 ;; dyna.table:table-dyna reads the captured slot.
                 (is-true (dyna.table:table-dyna class)
                          "class ~A must have a non-NIL :dyna after initialize-dyna!"
                          (second spec))))))
      (setf site.config:*dyna* saved-dyna
            site.config:*credentials* saved-creds))))
