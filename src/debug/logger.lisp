;;;; logger.lisp
;;;;
;;;; Structured logging and log processing.

(in-package #:aether)

;;; core logging definitions

(defparameter *logger* nil)
(setf (documentation *logger* 'variable)
      "Logging object that captures entries.")

(defparameter *log-level* 0)
(setf (documentation *log-level* 'variable)
      "A non-negative INTEGER that describes what level of logging to perform. Entries by default are given a log-level of 0, akin to DEBUG logging. Larger integers are used for logging more critical events.")

(defstruct logger
  (entries nil :type list))

;; NOTE: instead of storing raw property lists, we could wrap log entries in
;;     individualized structs.  this is a little more obnoxious in that one has
;;     to give definitions for all of the entries, but it has two upsides: you
;;     get typo safety against key misspellings, and you could perhaps install
;;     automatic 'conversion' routines which do things like, e.g., discard
;;     pointers to objects and retain only their public addresses.

(defun log-entry (&rest initargs
                  &key (logger *logger*) (log-level 0) source time entry-type
                  &allow-other-keys)
  "Injects a log entry."
  (declare (ignore source entry-type time))
  (when (and logger (>= log-level *log-level*))
    (let ((keys (copy-seq initargs)))
      (remf keys ':logger)
      (push keys (logger-entries logger))
      (values))))

(defun reset-logger (&optional (logger *logger*))
  "Empties the current logger of all entries."
  (when logger
    (setf (logger-entries logger) nil)))

(defmacro with-transient-logger ((&key (log-level 0)) &body body)
  "Initialize a fresh logger with the given `LOG-LEVEL'. Returns log contents on close."
  `(let ((*logger* (make-logger))
         (*log-level* ,log-level))
     (reset-logger)
     ,@body
     *logger*))

;;; pretty-printing mechanisms

(defgeneric print-log-entry (entry source entry-type &optional stream)
  (:documentation "Pretty-prints a log entry `ENTRY' to `STREAM'. Can be specialized on `SOURCE' or `ENTRY-TYPE' to add specific printing behavior.")
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
                            (entry-type (eql ':send-message))
                            &optional (stream *standard-output*))
  (format stream "~5f: ~a sending ~a to ~a:~%    ~a~%"
          (getf entry ':time)
          (getf entry ':source)
          (type-of (getf entry ':payload))
          (getf entry ':destination)
          (getf entry ':payload)))

;;; filtering mechanisms

(defun message-log (&optional (entries (logger-entries *logger*)))
  "Given a LIST of logger `ENTRIES', filter it so that it only contains `SEND-MESSAGE' entries. Re-sends are not included."
  (let (trimmed-entries message-ids)
    (dolist (entry entries (reverse trimmed-entries))
      (when (eql ':send-message (getf entry ':entry-type))
        (with-slots (message-id) (getf entry ':payload)
          (when (not (member message-id message-ids))
            (push message-id message-ids)
            (push entry trimmed-entries)))))))

(defun message-report (message-log-entries)
  "Given a `MESSAGE-LOG', which is a LIST of log entries filtered such that they only contain message-related logs, build an ALIST of the different types of sent messages it contains, and a count for each."
  (initialize-and-return (message-counts)
    (dolist (entry message-log-entries)
      (let ((payload-type (type-of (getf entry ':payload))))
        (incf (cdr (assoc-default payload-type message-counts 0)))))))

(defun trim-log (&key (entries (logger-entries *logger*))
                      (start-time 0)
                      (end-time most-positive-fixnum))
  "Trims log messages in the given LIST of log `ENTRIES' to only ones in between `START-TIME' and `END-TIME'."
  (let (trimmed-entries)
    (dolist (entry entries (reverse trimmed-entries))
      (when (and (<= start-time (getf entry ':time))
                 (>= end-time (getf entry ':time)))
        (push entry trimmed-entries)))))

(defun logs-for-process (process &optional (entries (logger-entries *logger*)))
  "Trims log `ENTRIES' to only ones produced by `PROCESS'."
  (let (trimmed-entries)
    (dolist (entry entries (reverse trimmed-entries))
      (when (eql (getf entry ':source) process)
        (push entry trimmed-entries)))))

(defun logs-for-address (address &optional (entries (logger-entries *logger*)))
  "Trims log `ENTRIES' to only ones produced by the process at public address `ADDRESS'."
  (let (trimmed-entries)
    (dolist (entry entries (reverse trimmed-entries))
      (when (address= (process-public-address (getf entry ':source)) address)
        (push entry trimmed-entries)))))

(defun logs-for-channel (channel &optional (entries (logger-entries *logger*)))
  "Trims log `ENTRIES' to only ones produced by the process with public address channel `CHANNEL'."
  (let (trimmed-entries)
    (dolist (entry entries (reverse trimmed-entries))
      (when (eql (address-channel (process-public-address (getf entry ':source)))
                 channel)
        (push entry trimmed-entries)))))

(defun logs-from-level (log-level &optional (entries (logger-entries *logger*)))
  "Trims log `ENTRIES' to only ones with a log-level >= `LOG-LEVEL'."
  (let (trimmed-entries)
    (dolist (entry entries (reverse trimmed-entries))
      (when (>= (getf entry ':log-level) log-level)
        (push entry trimmed-entries)))))

;; printer functions

(defun print-message-report (&optional (entries (logger-entries *logger*)))
  "Print a report of the different types of messages in the given LIST of log `ENTRIES', and a count for each. Calls `MESSAGE-LOG' and `MESSAGE-REPORT'."
  (let ((message-counts (message-report (message-log entries)))
        (total-count 0))
    (loop :for (message-type . num) :in message-counts
          :do (format t "~%~A: ~A" message-type num)
              (setf total-count (+ num total-count)))
    (format t "~%TOTAL: ~A" total-count)))

(defun print-log (&key (entries (logger-entries *logger*))
                       (stream *standard-output*)
                       (start-time nil)
                       (end-time nil)
                       (log-level nil))
  "Iterate through the entries in `LOGGER' in reverse order and print them to `STREAM' using `PRINT-LOG-ENTRY', which can be specialized on `SOURCE' and `ENTRY-TYPE'. For convenience, the `START-TIME', `END-TIME', and `LOG-LEVEL' keywords can be used to trim the log entries before printing."
  (let ((entries-to-print (copy-list entries)))
    (when start-time
      (setf entries-to-print (trim-log :entries entries-to-print :start-time start-time)))
    (when end-time
      (setf entries-to-print (trim-log :entries entries-to-print :end-time end-time)))
    (when log-level
      (setf entries-to-print (logs-from-level log-level entries-to-print)))
    (dolist (entry (reverse entries-to-print))
      (print-log-entry entry
                       (getf entry ':source)
                       (getf entry ':entry-type)
                       stream))))
