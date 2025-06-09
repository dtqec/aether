;;;; aether.asd
;;;;
;;;; Author: Eric Peterson, Peter Karalekas

(asdf:defsystem #:aether
  :description "A DSL for emulating an actor-based distributed system, housed on a family of emulated devices."
  :author "Eric Peterson <peterson.eric.c@gmail.com>, Peter Karalekas <peter@karalekas.com>"
  :version (:read-file-form "VERSION.txt")
  :license "MIT (See LICENSE.md)"
  :pathname "src/"
  :depends-on (#:alexandria
               #:policy-cond            ; used in DESTRUCTURING-PLACES
               #:cl-heap                ; simulation is founded on cl-heap:priority-queue
               #:global-vars            ; store DPU code
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:aether-tests)))
  :around-compile (lambda (compile)
                    (let (#+sbcl(sb-ext:*derive-function-types* t))
                      (funcall compile)))
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "queue")
               (:file "cheap-heap")
               (:module "debug"
                :serial t
                :components ((:file "logger")
                             (:file "trace")))
               (:file "event")
               (:file "message")
               (:file "courier")
               (:file "network")
               (:module "process"
                :serial t
                :components ((:file "process")
                             (:file "dpu-helpers")
                             (:file "dereference")
                             (:file "emissary")
                             (:file "rpc")
                             (:file "lock")
                             (:file "cast")))))
