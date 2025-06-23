;;;; aether-tests.asd
;;;;
;;;; Author: Eric Peterson, Peter Karalekas

(asdf:defsystem #:aether-tests
  :description "Regression tests for AETHER."
  :author "Eric Peterson <peterson.eric.c@gmail.com>, Peter Karalekas <peter@karalekas.com>"
  :depends-on (#:aether
               #:fiasco
               #:uiop
               )
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call ':aether-tests
                                           '#:run-aether-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:file "queue")
               (:file "cheap-heap")
               (:file "event")
               (:file "network")
               (:file "process")
               (:file "process-tree")
               (:file "cast")
               (:file "recursive-lock")
               (:file "logger")
               (:module "examples"
                :serial t
                :components ((:file "coloring")
                             (:file "flooding")
                             (:file "lock")
                             (:file "data-frame")
                             (:file "distributed-mst")
                             (:file "tree-coloring")
                             (:file "tree-operations")
                             ))))
