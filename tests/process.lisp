;;;; tests/process.lisp
;;;;
;;;; Tests which cover functionality of aether `PROCESS'es.

(in-package #:aether-tests)

;;;
;;; more complex test of event loop behavior when coupled to couriers, processes
;;;
;;; NOTE: I found myself scrolling up and down in matcher.lisp between the message
;;;       handler section and the process upkeep section in order to build my own
;;;       version for the chatterbox. Specifically, encoding the behavior of a
;;;       sync-receive in the commands section seemed a bit strange, as it differs
;;;       from the typical separation of concerns.
;;;

;; global variable for all chatterbox addresses

(defvar *chatterbox-addresses*)

;;; the processes

(defclass chatterbox (process)
  ((vote :initarg :vote :accessor vote)
   (pongs :initform 0 :accessor pongs)
   (tally :initform 0 :accessor tally)))

;;; the message definitions

(defstruct (msg-ping (:include message)))

(defstruct (msg-pong (:include message))
  (vote))

;; the message handlers

(define-message-handler handle-msg-ping
    ((box chatterbox) (message msg-ping) time)
  (with-slots (reply-channel) message
    (send-message reply-channel (make-msg-pong :vote (vote box)))))

;;; the message dispatch

(define-message-dispatch chatterbox
  (msg-ping   'handle-msg-ping))

;;; the process commands

(define-process-upkeep ((box chatterbox) time)
    (START)
  (process-continuation box
                        `(PING ,*chatterbox-addresses*)
                        `(WAIT)))

(define-process-upkeep ((box chatterbox) time)
    (PING addresses)
  (unless (endp addresses)
    (let* ((address (pop addresses))
           (listen-channel (register))
           (ping-message (make-msg-ping :reply-channel listen-channel)))
      (process-continuation box `(PING ,addresses))
      (send-message address ping-message)
      (sync-receive (listen-channel pong-message)
        (msg-pong
         (unregister listen-channel)
         (process-continuation box `(TALLY ,(msg-pong-vote pong-message))))))))

(define-process-upkeep ((box chatterbox) time)
    (TALLY vote)
  (incf (pongs box))
  (incf (tally box) vote))

(define-process-upkeep ((box chatterbox) time)
    (WAIT)
  (process-continuation box `(WAIT)))

(deftest test-chatterbox-consensus ()
  (let* ((count 11)
         (simulation (make-simulation))
         (*local-courier* (make-courier))
         *chatterbox-addresses*
         chatterboxes
         votes)
    ;; install the courier into the event loop
    (simulation-add-event simulation
                          (make-event :callback *local-courier*
                                      :time 0))
    ;; install the chatterboxes into the event loop
    (loop :for j :below count
          :for vote := (random 2)
          :for box := (spawn-process 'chatterbox :process-clock-rate 10
                                                 :vote vote)
          :do (simulation-add-event simulation
                                    (make-event :callback box :time 0))
              (push (process-public-address box) *chatterbox-addresses*)
              (push box chatterboxes)
              (push vote votes))
    (let ((proposed-consensus (round (/ (apply '+ votes) count))))
      (simulation-run simulation :canary (canary-until 15))
      (dolist (box chatterboxes)
        (is (= proposed-consensus (round (/ (tally box) (pongs box)))))))))
