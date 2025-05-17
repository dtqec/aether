;;;; logger.lisp
;;;;
;;;; Structured logging and log processing.

(in-package #:aether)

;;; core logging definitions

(defparameter *logger* nil)
(setf (documentation *logger* 'variable)
      "Logging object that captures entries.")

(defstruct logger
  (entries nil :type list))

;; NOTE: instead of storing raw property lists, we could wrap log entries in
;;     individualized structs.  this is a little more obnoxious in that one has
;;     to give definitions for all of the entries, but it has two upsides: you
;;     get typo safety against key misspellings, and you could perhaps install
;;     automatic 'conversion' routines which do things like, e.g., discard
;;     pointers to objects and retain only their public addresses.

(defun log-entry (&rest initargs &key (logger *logger*) source time entry-type &allow-other-keys)
  "Injects a log entry."
  (declare (ignore source entry-type time))
  (when logger
    (let ((keys (copy-seq initargs)))
      (remf keys ':logger)
      (push keys (logger-entries logger))
      (values))))

(defun reset-logger (&optional (logger *logger*))
  "Empties the current logger of all entries."
  (when logger
    (setf (logger-entries logger) nil)))

(defmacro with-transient-logger (() &body body)
  "Initialize a fresh logger.  Returns log contents on close."
  `(let ((*logger* (make-logger)))
     (reset-logger)
     ,@body
     *logger*))

;;; pretty-printing mechanisms

(defgeneric print-log-entry (entry source entry-type &optional stream)
  (:documentation "Pretty-prints a log entry to STREAM.")
  (:method (entry source entry-type &optional (stream *standard-output*))
    (let ((entry (copy-seq entry))
          (time (getf entry ':time))
          (source (getf entry ':source))
          (entry-type (getf entry ':entry-type)))
      (remf entry ':time)
      (remf entry ':source)
      (remf entry ':entry-type)
      (format stream "~5f: ~a logged ~a:~%    ~a~%"
              time source entry-type
              entry))))

(defmethod print-log-entry (entry
                            source
                            (entry-type (eql 'SEND-MESSAGE))
                            &optional (stream *standard-output*))
  (format stream "~5f: ~a sending ~a to ~a:~%    ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (type-of (getf entry ':payload))
          (getf entry ':destination)
          (getf entry ':payload)))

(defun print-log (entries &optional (stream *standard-output*))
  (dolist (entry entries)
    (print-log-entry entry
                     (getf entry ':source)
                     (getf entry ':entry-type)
                     stream)))

(defun message-log (logger)
  "Given a `LOGGER', filter it so that it only contains `SEND-MESSAGE' entries. Re-sends are not included."
  (let (entries message-ids)
    (dolist (entry (reverse (logger-entries logger)) (reverse entries))
      (when (eql 'SEND-MESSAGE (getf entry ':entry-type))
        (with-slots (message-id) (getf entry ':payload)
          (when (not (member message-id message-ids))
            (push message-id message-ids)
            (push entry entries)))))))

(defun message-report (message-log)
  "Given a `MESSAGE-LOG', which is a LIST of log entries filtered such that they only contain message-related logs, build an ALIST of the different types of sent messages it contains, and a count for each."
  (let (message-counts)
    (dolist (entry message-log)
      (let ((payload-type (type-of (getf entry ':payload))))
        (incf (cdr (assoc-default payload-type message-counts 0)))))
    message-counts))

(defun print-message-report (&optional (logger *logger*))
  "Print a report of the different types of messages sent in `LOGGER', and a count for each. Calls `MESSAGE-LOG' and `MESSAGE-REPORT'."
  (let ((message-counts (message-report (message-log logger)))
        (total-count 0))
    (loop :for (message-type . num) :in message-counts
          :do (format t "~%~A: ~A" message-type num)
              (setf total-count (+ num total-count)))
    (format t "~%TOTAL: ~A" total-count)))
