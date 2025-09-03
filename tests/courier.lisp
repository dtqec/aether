;;;; tests/courier.lisp
;;;;
;;;; Tests of courier.lisp functionality.

(in-package #:aether-tests)

(defstruct (message-fast (:include message))
  "Message type which gets processed quickly.")

(defstruct (message-slow (:include message))
  "Message type which gets processed slowly.")

(deftest test-message-processing-ordering ()
  "Test that clause ordering does not affect message queue traversal order."
  (flet ((pull-message (address)
           (let (first-message)
             (receive-message (address message)
               ;; process fast clause before slow clause
               (message-fast
                (setf first-message message))
               (message-slow
                (setf first-message message)))
             first-message)))
    (let* ((*local-courier* (make-courier))
           (address (register)))
      (with-simulation (simulation (*local-courier*))
        ;; insert slow message before fast message
        (send-message address (make-message-slow))
        (send-message address (make-message-fast))
        ;; check that *first* message is processed before *fast* message
        (is (typep (pull-message address) 'message-slow))
        (is (typep (pull-message address) 'message-fast))))))
