;;;; tests/recursive-lock.lisp
;;;;
;;;; Tests which cover the behavior of aether's prototypical recursive lock.

(in-package #:aether-tests)

(defparameter *locking-events* nil)

(defclass process-lockable-test (process-lockable process-tree)
  ()
  (:documentation "Testbed for aether's recursive lock."))

(defstruct (message-test-lock (:include message))
  "Demo message type. Supposed to be delayed by an established lock on a process.")

(define-message-handler handle-message-test-lock
    ((process process-lockable-test) (message message-test-lock))
  "Records the receipt of a test lock message."
  (push `(,(process-tree-id process) ,(now)) *locking-events*))

(defmethod process-lockable-targets ((process process-lockable-test))
  "See `PROCESS-TREE-CHILDREN' for more information."
  (process-tree-children process))

(define-message-dispatch process-lockable-test
  (message-lock        'handle-message-lock)
  (message-test-lock   'handle-message-test-lock
                       (not (process-lockable-locked? ; don't reply if locked
                             process-lockable-test))))

(deftest test-process-lockable-successful ()
  "Tests that message processing can be blocked by a recursive lock."
  (with-address-dereferencing () ; used by process-lockable-targets
    (with-courier ()
      (with-simulation (simulation (*local-courier*))
        (let ((*locking-events* nil)
              (processes (add-tree-processes simulation 'process-lockable-test))
              (rx-channel (register)))
          ;; establish a lock
          (send-message (process-public-address (nth 0 processes))
                        (make-message-lock :reply-channel rx-channel))
          ;; wait for the lock to establish
          (simulation-run simulation :canary (canary-until 20))
          (setf (simulation-horizon simulation)
                (max 20 (simulation-horizon simulation)))
          (receive-message (rx-channel locked-message)
            (message-rpc-done
             (let ((tx-channel (message-rpc-done-result locked-message)))
               ;; send a blocked message
               (with-active-simulation simulation
                 (send-message (process-public-address (nth 3 processes))
                               (make-message-test-lock)))
               ;; wait a bit
               (simulation-run simulation :canary (canary-until 30))
               ;; release the lock
               (with-active-simulation simulation
                 (send-message tx-channel (make-message-unlock)))
               ;; wait for the lock to release
               (simulation-run simulation :canary (canary-until 40))
               (receive-message (rx-channel released-message)
                 (message-rpc-done
                  (unregister rx-channel)
                  ;; check that the message has been processed
                  (loop :for (id time) :in *locking-events*
                        :do (is (< 20 time)))
                  ;; check that we aren't leaking addresses
                  (is (= 11 (hash-table-count
                             (aether::courier-inboxes *local-courier*)))))
                 (otherwise
                  (error "No unlock reply.")))))
            (otherwise
             (error "No lock reply."))))))))
