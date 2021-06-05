(defpackage invogen/tests/main
  (:use :cl
        :invogen
        :rove))
(in-package :invogen/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :invogen)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
