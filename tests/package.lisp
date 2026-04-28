(defpackage :site.tests
  (:use :cl :fiveam)
  (:export :all-tests))

(in-package :site.tests)

(def-suite all-tests
  :description "Top-level suite aggregating every site test.")
