;;;; params.lisp
;;;;
;;;; some global definitions to cope with file circularity

(in-package #:aether)

(defparameter *local-courier* nil
  "Bound to the `COURIER' that services this process.")
