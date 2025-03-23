;;;; lock.lisp
;;;;
;;;; Common commands defining a broadcast locking procedure for `PROCESS'es.
;;;;
;;;; In the subordinate (i.e., general) case, locks are established by sending a
;;;; `MESSAGE-LOCK-' RPC call to the target process.  Further locks are
;;;; recursively generated according to `PROCESS-LOCKABLE-TARGETS'.  The result of
;;;; this call is a channel, and sending `MESSAGE-UNLOCK' on it releases the
;;;; lock.  In the topmost case, a locking region is surrounded by the pair of
;;;; process commands `BROADCAST-LOCK' and `BROADCAST-UNLOCK'.
;;;;
;;;; The generic lock pattern follows four steps:
;;;;
;;;; (0) The Lock-Holder (H) and Client-Process (C) start out uninvolved.
;;;;
;;;;         C             H
;;;; 
;;;; (1) H sends a request to C, which carries as a payload the private channel
;;;;     PH.  From the perspective of H, PH is a DOWNWARD-RX-LATCH (i.e., H will
;;;;     be listening on it); from the perspective of C, it is an UPWARD-TX-LATCH
;;;;     (i.e., C will eventually send on it).
;;;;
;;;;         C <-lock-PH-- H
;;;;
;;;;     [This is performed recursively: C's job is then to request locks from
;;;;     all of its children, as defined by `PROCESS-LOCKABLE-TARGETS'.]
;;;;
;;;; (2) On success, C replies with a DONE message on PH, which carries as
;;;;     payload another private channel PC.  From the perspective of C, PC is
;;;;     the UPWARD-RX-LATCH (i.e., C will be listening on it); from the
;;;;     perspective of H, it is a DOWNWARD-TX-LATCH (i.e., H will send on it).
;;;;
;;;;                PH
;;;;           /---------\
;;;;       TX /           v RX
;;;;         C --done-PC-> H
;;;;
;;;; LOCKED REGION: H and C do whatever they need to do in the locked region.
;;;;        During this time, C busy-listens on PC for a RELEASE message, which
;;;;        signals H's intention to exit the locked region.
;;;;
;;;;                PH
;;;;           /---------\
;;;;       TX /           v RX
;;;;         C             H
;;;;       RX ^           / TX
;;;;           \---------/
;;;;                PC
;;;;
;;;; (3) When H is finished, it sends a RELEASE message along PC.  It then waits
;;;;     to hear a reply on PH, confirming the closure of the lock.
;;;;
;;;;                PH
;;;;           /---------\
;;;;       TX /           v RX
;;;;         C             H
;;;;       RX ^^         // TX
;;;;            =release=
;;;;                PC
;;;;
;;;; (4) C receives the signal on PC, and it exits the critical area.  At this
;;;;     point, PC serves no further purpose and is closed.  [It then recursively
;;;;     releases signals along any downward locks it holds.]  It sends a DONE
;;;;     message along PH, indicating that the lock has been released.
;;;;
;;;;                PH
;;;;            ===done==
;;;;       TX //         vv RX
;;;;         C             H
;;;;
;;;; At this point, PH serves no further purpose and is closed.  [Once all the
;;;; held locks PH receive their completion signal,] H and C proceed as before.
;;;;
;;;;         C             H
;;;;


(in-package #:aether)

;;;
;;; generic locking class
;;;

(defclass process-lockable (process)
  ((aborting?
    :accessor process-lockable-aborting?
    :initform nil
    :type boolean)
   (locked?
    :accessor process-lockable-locked?
    :initform nil
    :type boolean)
   (done-signal
    :accessor process-lockable-done-signal
    :initform nil)
   (downward-rx-latches
    :initform nil
    :type list)
   (downward-tx-latches
    :initform nil
    :type list)
   (upward-rx-latch
    :initform nil
    :type (or null address))
   (upward-tx-latch
    :initform nil
    :type (or null address)))
  (:documentation "A PROCESS which supports recursive locking."))

;;;
;;; generic functions to be specialized by processes performing recursive locks
;;;

(defgeneric process-lockable-targets (process-lockable)
  (:documentation "Calculates the list of ADDRESSes to acquire recursive locks from when PROCESS-LOCKABLE receives a lock request."))

;;;
;;; messages in the locking procedure
;;;

(defstruct (message-lock (:include message))
  "Sent to establish a lock.")

(defstruct (message-unlock (:include message))
  "Sent to release a lock."
  (result))

(define-message-handler handle-message-lock
    ((process process-lockable) (message message-lock))
  "Attempts to lock PROCESS."
  (with-slots (reply-channel) message
    (with-slots (aborting? locked?) process
      (cond
        (locked?
         (send-message reply-channel (make-message-rpc-done :result nil)))
        (t
         (setf locked? t)
         (process-continuation process `(START-LOCK ,reply-channel)))))))

(define-process-upkeep ((process process-lockable))
    (START-LOCK reply-channel)
  "Locks `PROCESS'.  `REPLY-CHANNEL' indicates the address (if any) on which to signal whether the lock was / was not established."
  (with-slots (aborting? done-signal locked? downward-rx-latches downward-tx-latches upward-rx-latch upward-tx-latch) process
    (setf aborting?           nil
          done-signal         nil
          locked?             t
          downward-rx-latches nil
          downward-tx-latches nil
          upward-rx-latch     nil
          upward-tx-latch     reply-channel)
    (process-continuation process
                          `(BROADCAST-LOCK ,(process-lockable-targets process))
                          `(%WAIT-FOR-UNLOCK)
                          `(BROADCAST-UNLOCK)
                          `(%FINISH-UNLOCK))))

(define-process-upkeep ((process process-lockable))
    (BROADCAST-LOCK targets)
  "Establishes locks on `TARGETS'."
  (with-slots (aborting? downward-rx-latches downward-tx-latches
               upward-rx-latch upward-tx-latch)
      process
    (unless aborting?
      (let ((new-downward-rx-latches
              (send-message-batch #'make-message-lock targets)))
        (with-replies (new-downward-tx-latches :returned? returned? :close? nil)
                      new-downward-rx-latches
          (when (and (not aborting?) returned?)
            (log-entry :entry-type 'aborting-lock
                       :reason 'broadcast-lock-returned
                       :returned? returned?))
          (setf aborting? (or aborting? returned?))
          (loop :for downward-rx-latch :in new-downward-rx-latches
                :for downward-tx-latch :in new-downward-tx-latches
                :do (cond
                      (downward-tx-latch
                       (push downward-tx-latch downward-tx-latches)
                       (push downward-rx-latch downward-rx-latches))
                      (t
                       (unregister downward-rx-latch)
                       (log-entry :entry-type 'aborting-lock
                                  :reason 'no-downward-tx-latch)
                       (setf aborting? t))))
          (when upward-tx-latch
            (setf upward-rx-latch (unless aborting? (register)))
            (send-message upward-tx-latch
                          (make-message-rpc-done :result upward-rx-latch))))))))

(define-process-upkeep ((process process-lockable))
    (%WAIT-FOR-UNLOCK)
  "Waits for a release signal to arrive via `UPWARD-RX-LATCH'."
  (with-slots (aborting? done-signal upward-rx-latch) process
    (when (and upward-rx-latch (not aborting?))
      (sync-receive (upward-rx-latch done-message)
        (message-unlock
         (log-entry :entry-type 'completed-wait-for
                    :channel upward-rx-latch)
         (setf done-signal     (message-unlock-result done-message)
               upward-rx-latch (unregister upward-rx-latch)))))))

;; TODO: this (and %FINISH-UNLOCKS) ought to be paired by a "context macro"
(define-process-upkeep ((process process-lockable))
    (BROADCAST-UNLOCK &key &allow-other-keys)
  "Cleans up after BROADCAST-LOCK."
  (with-slots (aborting? done-signal downward-rx-latches downward-tx-latches upward-tx-latch) process
    (send-message-batch (a:curry #'make-message-unlock :result done-signal)
                        downward-tx-latches
                        :replies? nil)
    (with-replies (replies) downward-rx-latches
      (setf aborting? nil
            downward-tx-latches nil
            downward-rx-latches nil)
      (when upward-tx-latch
        (send-message upward-tx-latch (make-message-rpc-done :result t))))))

(define-process-upkeep ((process process-lockable))
    (%FINISH-UNLOCK)
  "Cleans up after START-LOCK."
  (with-slots (aborting? done-signal locked? downward-rx-latches downward-tx-latches upward-rx-latch upward-tx-latch) process
    (setf aborting?           nil
          done-signal         nil
          locked?             nil
          downward-rx-latches nil
          downward-tx-latches nil
          upward-rx-latch     nil
          upward-tx-latch     nil)))
