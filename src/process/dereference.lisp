;;;; process/dereference.lisp
;;;;
;;;; Debug apparatus for tracking all processes spawned during simulation.

(in-package #:aether)

(defparameter *dereferencing-table* nil
  "Holds a hash map from ADDRESS objects to the Lisp objects that own them.")

(defun record-reference (process)
  "If appropriate, creates a debug record that PROCESS owns its public address."
  (when *dereferencing-table*
    (setf (gethash (process-public-address process) *dereferencing-table*)
          process))
  process)

(defmacro with-address-dereferencing (() &body body)
  "Context macro which permits use of DEREFERENCE in its body."
  (a:with-gensyms (old-table)
    `(let* ((,old-table *dereferencing-table*)
            (*dereferencing-table* (if ,old-table ,old-table
                                       (make-hash-table :hash-function #'hash-address
                                                        :test #'address=))))
       ,@body)))

(defun dereference (address)
  "Looks up the Lisp PROCESS object which owns the provided public ADDRESS.  Only functions within WITH-ADDRESS-DEREFERENCING, not hardware-realistic, only for use during debugging."
  (unless *dereferencing-table*
    (error "DEREFERENCE only works in the context of WITH-ADDRESS-DEREFERENCING."))
  (check-type address address)
  (initialize-and-return
      ((value (gethash address *dereferencing-table*)))
    (unless value
      (error "Unknown address: ~a" address))))

(defun spawn-process (class &rest initargs)
  "Preferred mechanism for instantiating an object inherting from PROCESS.  Contains debug hooks."
  (initialize-and-return
      ((process (apply #'make-instance class initargs)))
    (record-reference process)))
