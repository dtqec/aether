;;;; trace.lisp
;;;;
;;;; Utilities for managing RPC trace output.

(in-package #:aether)

(defparameter *message-traces* nil
  "Houses information related to traces.")

(defstruct tracer
  "Collects trace information.

TRACES: ORIGIN-TYPE --ALIST-> MESSAGE-TYPE --ALIST-> D-COUNT --ALIST-> DELTAs."
  (traces nil))

(defun tracer-store (&rest initargs)
  "Add an entry to the set of traces."
  (unless (null *message-traces*)
    (destructuring-bind (&key delta origin message destination-count &allow-other-keys)
        initargs
      (let* ((messages-as-cdr (assoc-default origin (tracer-traces *message-traces*) nil))
             (dcounts-as-cdr (assoc-default message (cdr messages-as-cdr) nil))
             (counts-as-cdr (assoc-default destination-count (cdr dcounts-as-cdr) nil)))
        (push delta (cdr counts-as-cdr))))))

(defmacro with-traces (() &body body)
  "Enable tracing over a region of code."
  `(let ((*message-traces* (make-tracer)))
     ,@body))

(defun sort-traces ()
  "Sort the contents of a TRACER object.  Useful for printing."
  (unless (null *message-traces*)
    (setf (tracer-traces *message-traces*)
          (sort (tracer-traces *message-traces*) #'string< :key #'car))
    (loop :for (_ . rest) :in (tracer-traces *message-traces*)
          :do (setf rest (sort rest #'string< :key #'car))
              (loop :for (_ . restrest) :in rest
                    :do (setf restrest (sort restrest #'< :key #'car))))))

(defun print-traces (&optional (stream *standard-output*))
  "Dump the contents of *MESSAGE-TRACES* to STREAM."
  (unless (null *message-traces*)
    (sort-traces)
    (loop :for (origin . messages) :in (tracer-traces *message-traces*)
          :do (format stream "ORIGIN: ~a~%" origin)
              (loop :for (message . dcounts) :in messages
                    :do (format stream "  MESSAGE: ~a~%" message)
                        (loop :for (dcount . hits) :in dcounts
                              :do (format stream "    @ ~2d: ~5d, ~5f max, ~5f avg, ~5f sigma~%"
                                          dcount (length hits) (reduce #'max hits)
                                          (a:mean hits) (a:standard-deviation hits)))))))

(defun rotate-trace-table (table)
  "Takes TABLE, an ALIST associating code distances to TRACER objects, and produces a TRACER-type nested ALIST whose innermost nesting is an ALIST associating code distances to triples (AVERAGE MAX COUNT)."
  (initialize-and-return (rotated-traces)
    ;; rotate traces
    (dolist (cd-entry table)
      (dolist (origin-entry (tracer-traces (cdr cd-entry)))
        (dolist (message-entry (cdr origin-entry))
          (dolist (dcount-entry (cdr message-entry))
            (let* (;; extract
                   (code-distance (car cd-entry))
                   (origin (car origin-entry))
                   (message (car message-entry))
                   (dcount (car dcount-entry))
                   (hits (cdr dcount-entry))
                   ;; compute
                   (average (a:mean hits))
                   (max (reduce #'max hits))
                   (count (length hits))
                   ;; store
                   (origin-cubby (assoc-default origin rotated-traces nil))
                   (message-cubby (assoc-default message (cdr origin-cubby) nil))
                   (dcount-cubby (assoc-default dcount (cdr message-cubby) nil))
                   (cd-cubby (assoc-default code-distance (cdr dcount-cubby) nil)))
              (setf (cdr cd-cubby) (list average max count)))))))
    ;; fill out empty traces
    (dolist (origin-entry rotated-traces)
      (dolist (message-entry (cdr origin-entry))
        (dolist (dcount-entry (cdr message-entry))
          (dolist (cd-entry table)
            (let ((cubby (assoc-default (car cd-entry) (cdr dcount-entry) nil)))
              (unless (cdr cubby)
                (setf (cdr cubby) (list nil nil nil)))))
          ;; sort the entries on the way out
          (setf (cdr dcount-entry) (sort (cdr dcount-entry) #'< :key #'car)))
        (setf (cdr message-entry) (sort (cdr message-entry) #'< :key #'car)))
      (setf (cdr origin-entry) (sort (cdr origin-entry) #'string< :key #'car)))
    (setf rotated-traces (sort rotated-traces #'string< :key #'car))))

(defun print-trace-table (table &optional (stream *standard-output*))
  "Prints TABLE, an ALIST associating code distances to TRACER objects, to STREAM."
  (let ((rotated-traces (rotate-trace-table table)))
    (dolist (origin-entry rotated-traces)
      (format stream "ORIGIN: ~a~%" (car origin-entry))
      (dolist (message-entry (cdr origin-entry))
        (format stream "  MESSAGE: ~a~%" (car message-entry))
        (dolist (dcount-entry (cdr message-entry))
          (format stream "    COUNT ~2d: " (car dcount-entry))
          (dolist (count-entry (cdr dcount-entry))
            (destructuring-bind (average max count) (cdr count-entry)
              (cond
                (average
                 (format stream "| ~4d x ~5f < ~5f " count average max))
                (t
                 (format stream "|  NIL x   NIL <   NIL ")))))
          (format stream "~%"))))))
