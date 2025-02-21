;;;; tests/cast.lisp
;;;;
;;;; Basic tests of broadcast/convergecast functionality.

(in-package #:aether-tests)

(defparameter *broadcast-events* nil)

(defclass process-tree-cast-test (process-tree)
  ()
  (:documentation "Testbed for aether broadcast & convergecast."))

;;;
;;; message definitions
;;;

(defstruct (broadcast-test (:include message))
  "Test message for demonstrating broadcast.")

(defstruct (broadcast-test-abort (:include message))
  "Test message for demonstrating broadcast with aborting."
  (abort-id))

(defstruct (broadcast-test-script (:include message))
  "Test message for demonstrating broadcast with a script."
  (scalar))

(defstruct (broadcast-test-rts (:include message))
  "Test message for demonstrating broadcast with RTS.")

(defstruct (convergecast-test (:include message))
  "Test message for demonstrating convergecast.")

(defstruct (convergecast-test-abort (:include message))
  "Test message for demonstrating convergecast with aborting."
  (abort-id))

(defstruct (convergecast-test-script (:include message))
  "Test message for demonstrating convergecast with a script."
  (scalar))

(defstruct (convergecast-test-rts (:include message))
  "Test message for demonstrating convergecast with RTS.")

;;;
;;; message handlers
;;;

(define-broadcast-handler handle-broadcast-test
    ((process process-tree-cast-test) (message broadcast-test) now)
  "Pushes the `PROCESS's ID to a global list."
  (push (process-tree-id process) *broadcast-events*)
  (push-broadcast-frame :targets (process-tree-children process)))

(define-broadcast-handler handle-broadcast-test-abort
    ((process process-tree-cast-test) (message broadcast-test-abort) now)
  "Pushes the `PROCESS's ID to a global list, and potentially aborts the broadcast if the `PROCESS's ID is equal to `ABORT-ID'."
  (with-slots (abort-id) message
    (push (process-tree-id process) *broadcast-events*)
    (push-broadcast-frame :targets (process-tree-children process))
    (when (= abort-id (process-tree-id process))
      (return-from-cast))))

(define-broadcast-handler handle-broadcast-test-script
    ((process process-tree-cast-test) (message broadcast-test-script) now)
  "Pushes the `:PUSH-TIMES' command onto the stack."
  (with-slots (scalar) message
    (process-continuation process `(PUSH-TIMES ,scalar))
    (push-broadcast-frame :targets (process-tree-children process))))

(define-broadcast-handler handle-broadcast-test-rts
    ((process process-tree-cast-test) (message broadcast-test-rts) now)
  "Pushes the `PROCESS's ID to a global list."
  (push (process-tree-id process) *broadcast-events*)
  (push-broadcast-frame :targets (process-tree-children process)
                        :handle-rts? t))

(define-convergecast-handler handle-convergecast-test
    ((process process-tree-cast-test) (message convergecast-test) now)
  "Puts the `PROCESS's ID as `INPUT' to the convergcast frame."
  (push-convergecast-frame :targets (process-tree-children process)
                           :func #'aether::reduce+
                           :input (process-tree-id process)))

(define-convergecast-handler handle-convergecast-test-abort
    ((process process-tree-cast-test) (message convergecast-test-abort) now)
  "Puts the `PROCESS's ID as `INPUT' to the convergecast frame, unless it is equal to `ABORT-ID', which triggers a `RETURN-FROM-CAST' and thus an abort of the convergecast operation."
  (with-slots (abort-id) message
    (push-convergecast-frame :targets (process-tree-children process)
                             :func #'aether::reduce+
                             :input (process-tree-id process))
    (when (= abort-id (process-tree-id process))
      (return-from-cast 0))))

(define-convergecast-handler handle-convergecast-test-script
    ((process process-tree-cast-test) (message convergecast-test-script) now)
  "Pushes the `:SET-TIMES' command onto the stack."
  (with-slots (scalar) message
    (process-continuation process `(SET-TIMES ,scalar))
    (push-convergecast-frame :targets (process-tree-children process)
                             :func #'aether::reduce+
                             :input (process-tree-id process))))

(define-convergecast-handler handle-convergecast-test-rts
    ((process process-tree-cast-test) (message convergecast-test-rts) now)
  "Puts the `PROCESS's ID as `INPUT' to the convergcast frame. In order to handle RTSes gracefully, our `FUNC' must also handle NILs."
  (labels ((null+ (input replies)
             (loop :for value :in (list* input replies)
                   :when value
                     :sum value :into subtotal
                   :finally (return subtotal))))
    (push-convergecast-frame :targets (process-tree-children process)
                             :func #'null+
                             :input (process-tree-id process)
                             :handle-rts? t)))

;;;
;;; command definitions
;;;

(define-process-upkeep ((process process-tree-cast-test) now)
    (PUSH-TIMES scalar)
  "Pushes the `PROCESS's ID times `SCALAR' onto `*BROADCAST-EVENTS*'."
  (push (* scalar (process-tree-id process)) *broadcast-events*))

(define-process-upkeep ((process process-tree-cast-test) now)
    (SET-TIMES scalar)
  "Sets the `INPUT' slot value of the data frame at the top of `PROCESS-DATA-STACK' equal to itself times `SCALAR'."
  (setf (slot-value (peek (process-data-stack process)) 'aether::input)
        (* scalar (slot-value (peek (process-data-stack process)) 'aether::input))))

(define-process-upkeep ((process process-tree-cast-test) now)
    (HALT)
  "Kills the process."
  (process-die))

;;;
;;; message dispatch

(define-message-dispatch process-tree-cast-test
  (broadcast-test            'handle-broadcast-test)
  (broadcast-test-abort      'handle-broadcast-test-abort)
  (broadcast-test-script     'handle-broadcast-test-script)
  (broadcast-test-rts        'handle-broadcast-test-rts)
  (convergecast-test         'handle-convergecast-test)
  (convergecast-test-abort   'handle-convergecast-test-abort)
  (convergecast-test-script  'handle-convergecast-test-script)
  (convergecast-test-rts     'handle-convergecast-test-rts))

;;;
;;; test definitions
;;;

(deftest test-process-broadcast ()
  "Simple test of broadcast facilities. In the first case, checks to make sure that all IDs are announced in `*BROADCAST-EVENTS*' in the correct order. In the second (aborting) case, checks to make sure that all IDs other than those of `ABORT-ID's children are announced in `*BROADCAST-EVENTS*', and in the correct order. In the third (scripting) case, we expect all IDs (each multiplied by 2) to appear in `*BROADCAST-EVENTS*', with children coming before parents. In the fourth (RTS) case, we expect all IDs other than 10, as it has been killed at the beginning of the simulation. See `ADD-TREE-PROCESSES' for the fixture used to build the tree of processes."
  (with-address-dereferencing ()
    (with-courier ()
      (with-simulation (simulation (*local-courier*))
        (let ((processes (add-tree-processes simulation 'process-tree-cast-test))
              (rx-channel (register))
              (scalar 2))
          (flet ((*-scalar (x)
                   (* scalar x))
                 (remove-all (items l)
                   (dolist (i items l)
                     (setf l (remove i l)))))
            (loop :for message :in (list
                                    (make-broadcast-test
                                     :reply-channel rx-channel)
                                    (make-broadcast-test-abort
                                     :reply-channel rx-channel
                                     :abort-id 2)
                                    (make-broadcast-test-script
                                     :reply-channel rx-channel
                                     :scalar scalar)
                                    (make-broadcast-test-rts
                                     :reply-channel rx-channel))
                  :for answer :in (list
                                   (reverse (alexandria:iota 11))
                                   (remove-all '(6 7 10)
                                               (reverse (alexandria:iota 11)))
                                   ;; Events don't necessarily come in ID order
                                   ;; when using a script, we just are guaranteed
                                   ;; that parents come before children.
                                   (mapcar #'*-scalar '(10 9 8 7 6 5 4 3 2 1 0))
                                   ;; For this test we kill process 10, so it is
                                   ;; not included in the ledger.
                                   '(9 8 7 6 5 4 3 2 1 0))
                  :for time :in (list 10 20 40 50)
                  :for kill? :in (list nil nil nil T)
                  :do (setf *broadcast-events* nil)
                      (when kill?
                        (setf (process-command-stack (nth 10 processes)) '((HALT))))
                      (simulation-run simulation
                                      :canary (canary-until
                                               (1+ (simulation-horizon simulation))))
                      (aether::with-active-simulation simulation
                        (send-message (process-public-address (nth 0 processes)) message))
                      (simulation-run simulation :canary (canary-until time))
                      (receive-message (rx-channel broadcast-ack)
                        (message-rpc-done
                         (is (equal *broadcast-events* answer)))
                        (otherwise
                         (error "No acknowledgement received."))))))))))

(deftest test-process-convergecast ()
  "Simple test of convergecast facilities, by collecting the sum of process IDs. In the first case we expect 55, which is the sum of 0 through 10. In the second (aborting) case, we expect 30, which is the sum of all IDs other than 2 and its children. In the third (scripting) case we expect 110, which is twice 55. In the fourth (RTS) case we expect 45, which is the sum of 0 through 9 (as process 10 is killed). See `ADD-TREE-PROCESSES' for the fixture used to build the tree of processes."
  (with-address-dereferencing ()
    (with-courier ()
      (with-simulation (simulation (*local-courier*))
        (let ((processes (add-tree-processes simulation 'process-tree-cast-test))
              (rx-channel (register))
              (scalar 2))
          (loop :for message :in (list
                                  (make-convergecast-test
                                   :reply-channel rx-channel)
                                  (make-convergecast-test-abort
                                   :reply-channel rx-channel
                                   :abort-id 2)
                                  (make-convergecast-test-script
                                   :reply-channel rx-channel
                                   :scalar scalar)
                                  (make-convergecast-test-rts
                                   :reply-channel rx-channel))
                :for answer :in (list
                                 (reduce '+ (alexandria:iota 11))
                                 (reduce '+ '(0 1 3 4 5 8 9))
                                 (* scalar (reduce '+ (alexandria:iota 11)))
                                 (reduce '+ (alexandria:iota 10)))
                :for time :in (list 10 20 40 50)
                :for kill? :in (list nil nil nil T)
                :do (when kill?
                      (setf (process-command-stack (nth 10 processes)) '((HALT))))
                    (simulation-run simulation
                                    :canary (canary-until
                                             (1+ (simulation-horizon simulation))))
                    (aether::with-active-simulation simulation
                        (send-message (process-public-address (nth 0 processes)) message))
                    (simulation-run simulation :canary (canary-until time))
                    (receive-message (rx-channel convergecast-response)
                      (message-rpc-done
                       (let ((result (message-rpc-done-result convergecast-response)))
                         (is (= result answer))))
                      (otherwise
                       (error "No result received.")))))))))
