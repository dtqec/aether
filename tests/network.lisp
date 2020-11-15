;;;; tests/network.lisp

(in-package #:aether-tests)

(deftest test-courier-grid ()
  "Instantiates a grid of couriers, opens a mailbox on each, and sends an all-to-all batch of messages."
  ;; set up couriers
  (let* ((grid-width 5)
         (couriers (make-courier-grid grid-width grid-width))
         (mailboxes (make-array (list grid-width grid-width)))
         (simulation (make-simulation))
         (time-step (/ (courier-processing-clock-rate (aref couriers 0 0))))
         (time-limit 10))
    ;; set up mailboxes
    (dotimes (i grid-width)
      (dotimes (j grid-width)
        (let ((*local-courier* (aref couriers i j)))
          (simulation-add-event simulation
                                (make-event
                                 :callback (aref couriers i j)))
          (setf (aref mailboxes i j) (register)))))
    ;; drop messages into system
    (dotimes (i grid-width)
      (dotimes (j grid-width)
        (let ((*local-courier* (aref couriers i j)))
          (dotimes (k grid-width)
            (dotimes (l grid-width)
              (send-message (aref mailboxes k l)
                            (make-message)))))))
    ;; run the simulation 'til we see n^2 messages in every mailbox
    (labels ((depths ()
               (let ((queue-depth 0)
                     (messages-received (* grid-width grid-width)))
                 (dotimes (i grid-width)
                   (dotimes (j grid-width)
                     (let ((out-queue (aether::courier-queue (aref couriers i j)))
                           (in-queue (gethash (aether::address-channel (aref mailboxes i j))
                                              (aether::courier-inboxes (aref couriers i j)))))
                       (alexandria:minf messages-received (q-len in-queue))
                       (alexandria:maxf queue-depth (q-len out-queue)))))
                 (values messages-received queue-depth)))
             (try-til-exhausted (stopping-time &optional (pressure-so-far 0))
               (simulation-run simulation :canary (canary-until stopping-time))
               (multiple-value-bind (receipts pressure) (depths)
                 (cond
                   ((= receipts (* grid-width grid-width))
                    (format t "~&Maximum network pressure: ~a"
                            (max pressure pressure-so-far))
                    (is (zerop pressure)))
                   ((> stopping-time time-limit)
                    nil)
                   (t
                    (try-til-exhausted (+ stopping-time time-step)
                                       (max pressure pressure-so-far)))))))
      (try-til-exhausted 0))))
