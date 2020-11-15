;;;; tests/examples/lock.lisp
;;;;
;;;; Simple example of mutual exclusion.

(in-package #:aether-tests)

;;; classes

(defclass process-writer (process-lockable)
  ((process-clock-rate :initform 1)
   (transmit-list
    :initarg :transmit-list
    :accessor process-writer-transmit-list)
   (target
    :initarg :target
    :accessor process-writer-target))
  (:documentation "Sends packets to a PROCESS-READER."))

(defclass process-reader (process-lockable)
  ((process-clock-rate :initform 1)
   (receive-list
    :initform nil
    :accessor process-reader-receive-list))
  (:documentation "Records packets captured from a PROCESS-WRITER."))

(defstruct (message-write (:include message))
  "A single packet, sent from a PROCESS-WRITER to a PROCESS-READER."
  payload)

;;; reader definitions

(define-process-upkeep ((process process-reader) now) (START)
  (process-continuation process `(START)))

(define-rpc-handler handle-message-write ((process process-reader) (message message-write) now)
  (push (message-write-payload message)  (process-reader-receive-list process)))

(defmethod process-lockable-targets ((process process-reader))
  nil)

;;; writer definitions

(define-process-upkeep ((process process-writer) now) (START)
  (unless (endp (process-writer-transmit-list process))
    (process-continuation process
                          `(BROADCAST-LOCK (,(process-writer-target process)))
                          `(TRANSMIT)
                          `(BROADCAST-UNLOCK)
                          `(START))))

(define-process-upkeep ((process process-writer) now) (TRANSMIT)
  (unless (or (process-lockable-aborting? process)
              (endp (process-writer-transmit-list process)))
    (send-message (process-writer-target process)
                  (make-message-write :payload (pop (process-writer-transmit-list process))))
    (process-continuation process `(TRANSMIT))))

(define-process-upkeep ((process process-writer) now) (START-NO-LOCKS)
  "This is a _bad_ version of START, to see what happens without locking."
  (unless (endp (process-writer-transmit-list process))
    (process-continuation process
                          `(TRANSMIT)
                          `(START-NO-LOCKS))))

(define-message-dispatch process-writer
  ;; writers don't have to receive any messages
  )

(define-message-dispatch process-reader
  (message-lock  'handle-message-lock)
  (message-write 'handle-message-write))

(defmethod process-lockable-targets ((process process-writer))
  nil)

;;; test for avoided race condition

(deftest test-race-condition-resolved ()
  "Spawns a reader process and some writers, sets them loose, and checks that the resulting pattern of packets seen by the reader are not interleaved."
  (let* ((couriers (make-courier-grid 1 1))
         (*local-courier* (aref couriers 0 0))
         (simulation (make-simulation))
         (reader (spawn-process 'process-reader))
         (writer-A (spawn-process 'process-writer
                                  :transmit-list '(A A A)
                                  :target (process-public-address reader)))
         (writer-B (spawn-process 'process-writer
                                  :transmit-list '(B B B)
                                  :target (process-public-address reader))))
    (loop :for item :in (list *local-courier* reader writer-A writer-B)
          :do (simulation-add-event simulation (make-event :callback item)))
    (simulation-run simulation :canary (canary-all (canary-process writer-A)
                                                   (canary-process writer-B)))
    (format t ": ~a" (process-reader-receive-list reader))
    (loop :for item :in (process-reader-receive-list reader)
          :for index :from 0
          :when (eq item 'A)
            :collect index :into A-positions
          :when (eq item 'B)
            :collect index :into B-positions
          :finally
             (is (or (< (car (last A-positions)) (first B-positions))
                     (< (car (last B-positions)) (first A-positions)))))))

(deftest test-race-condition-present ()
  "Spawns a reader process and some writers, tells the writers NOT TO LOCK, sets them loose, and checks that the resulting pattern of packets seen by the reader ARE interleaved."
  (let* ((couriers (make-courier-grid 1 1))
         (*local-courier* (aref couriers 0 0))
         (simulation (make-simulation))
         (reader (spawn-process 'process-reader))
         (writer-A (spawn-process 'process-writer
                                  :transmit-list '(A A A)
                                  :target (process-public-address reader)))
         (writer-B (spawn-process 'process-writer
                                  :transmit-list '(B B B)
                                  :target (process-public-address reader))))
    (loop :for item :in (list *local-courier* reader writer-A writer-B)
          :do (simulation-add-event simulation (make-event :callback item)))
    (setf (process-command-stack writer-A) `((START-NO-LOCKS))
          (process-command-stack writer-B) `((START-NO-LOCKS)))
    (simulation-run simulation :canary (canary-all (canary-process writer-A)
                                                   (canary-process writer-B)))
    (format t ": ~a" (process-reader-receive-list reader))
    (loop :for item :in (process-reader-receive-list reader)
          :for index :from 0
          :when (eq item 'A)
            :collect index :into A-positions
          :when (eq item 'B)
            :collect index :into B-positions
          :finally
             (is (and (>= (car (last A-positions)) (first B-positions))
                      (>= (car (last B-positions)) (first A-positions)))))))
