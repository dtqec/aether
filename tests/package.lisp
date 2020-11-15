;;;; tests/package.lisp

(fiasco:define-test-package #:aether-tests
  (:use #:aether)

  ;; suite.lisp
  (:export
   #:run-aether-tests))
