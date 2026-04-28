(in-package :site.tests)

(in-suite all-tests)

(test smoke
  "Empty smoke test — verifies the suite loads and runs."
  (is (= 2 (+ 1 1))))
