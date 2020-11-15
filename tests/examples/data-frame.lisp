;;;; tests/examples/data-frame.lisp

(in-package #:aether-tests)

(defclass arithmetic-server (process)
  ((process-clock-rate :initform 1))
  (:documentation "Simple server process that handles some arithmetic RPC calls."))

;;; basic services

(define-process-upkeep ((process arithmetic-server) now) (START)
  "This makes the server sit in an infinite \"listening\" loop."
  (process-continuation process `(START)))

(define-process-upkeep ((process arithmetic-server) now) (PUSH n)
  "Puts an item at the top of the data stack."
  (push n (process-data-stack process)))

(define-process-upkeep ((process arithmetic-server) now) (MULTIPLY)
  "Multiplies the top two elements on the data stack."
  (let ((left (pop (process-data-stack process)))
        (right (pop (process-data-stack process))))
    (push (* left right) (process-data-stack process))))

(define-process-upkeep ((process arithmetic-server) now) (EMIT address)
  "Sends the top element on the data stack to the indicated address."
  (let ((result (pop (process-data-stack process))))
    (send-message address (make-message-rpc-done :result result))))

;;; factorial, non-tail-call version

(defstruct (message-factorial (:include message))
  "Requests the computation of n!."
  (n   nil :type (integer 0)))

(define-message-handler handle-message-factorial
    ((process arithmetic-server) (message message-factorial) now)
  "Sets up the computation of n! to service an inbound request."
  (process-continuation process
                        `(FACTORIAL-STEP ,(message-factorial-n message))
                        `(EMIT ,(message-reply-channel message))))

(define-process-upkeep ((process arithmetic-server) now)
    (FACTORIAL-STEP n)
  "The workhorse for the non-tail-call version of factorial.

If appropriate, it first calls FACTORIAL-STEP recursively, which sets up a return value for (n-1)!. Then, it modifies that return value by pushing n onto the stack and multiplying."
  (cond
    ((zerop n)
     (process-continuation process `(PUSH 1)))
    (t
     (process-continuation process
                           `(FACTORIAL-STEP ,(1- n))
                           `(PUSH ,n)
                           `(MULTIPLY)))))

;;; factorial, tail-call version

(defstruct (message-factorial-tco (:include message-factorial))
  "Also a factorial query, handled slightly differently by the server.")

(define-message-handler handle-message-factorial-tco
    ((process arithmetic-server) (message message-factorial-tco) now)
  "Sets up the computation of n! to service an inbound request."
  (process-continuation process
                        `(PUSH 1)
                        `(FACTORIAL-STEP-TCO ,(message-factorial-n message))
                        `(EMIT ,(message-reply-channel message))))

(define-process-upkeep ((process arithmetic-server) now)
    (FACTORIAL-STEP-TCO n)
  "The workhorse for the tail-call version of factorial.

This assumes that the return-value has already been set up, contributes the factor of n, and remits the contribution of (n-1)! to a recursive call."
  (unless (zerop n)
    (process-continuation process
                          `(PUSH ,n)
                          `(MULTIPLY)
                          `(FACTORIAL-STEP-TCO ,(1- n)))))

;;; handlers

(define-message-dispatch arithmetic-server
  (message-factorial-tco 'handle-message-factorial-tco)
  (message-factorial     'handle-message-factorial))

;;;
;;; tests
;;;

;;; test 1: check that these compute what they're supposed to

(deftest test-factorial-validity ()
  ;; set up bare server
  (let* ((*local-courier* (make-courier))
         (simulation (make-simulation))
         (server (spawn-process 'arithmetic-server))
         (clock 0))
    (dolist (item (list *local-courier* server))
      (simulation-add-event simulation (make-event :callback item)))
    ;; DRY for querying the server
    (flet ((factorial-request (n &key tco?)
             (let* ((reply-address (register))
                    (payload (funcall (if tco?
                                          #'make-message-factorial-tco 
                                          #'make-message-factorial)
                                      :reply-channel reply-address
                                      :n n)))
               (send-message (process-public-address server) payload)
               reply-address))
           (factorial-await (listen-address)
             (let ((max-data-depth 0)
                   (max-command-depth 0))
               (loop
                 (incf clock)
                 (simulation-run simulation :canary (canary-until clock))
                 (alexandria:maxf max-data-depth (length (process-data-stack server)))
                 (alexandria:maxf max-command-depth (length (process-command-stack server)))
                 (receive-message (listen-address done-message)
                   (message-rpc-done
                    (return (values (message-rpc-done-result done-message)
                                    max-data-depth max-command-depth))))))))
      ;; try non-tco factorial
      (let ((old-clock clock))
        (multiple-value-bind (result max-data-depth max-command-depth)
            (factorial-await (factorial-request 10))
          (is (= 3628800 result))
          (format t "~&Non-TCO: ~2d clock, ~2d data depth, ~2d command depth"
                  (- clock old-clock) max-data-depth max-command-depth)))
      ;; send factorial tco message
      (let ((old-clock clock))
        (multiple-value-bind (result max-data-depth max-command-depth)
            (factorial-await (factorial-request 10 :tco? t))
          (is (= 3628800 result))
          (format t "~&TCO:     ~2d clock, ~2d data depth, ~2d command depth"
                  (- clock old-clock) max-data-depth max-command-depth)))
      ;; check server is currently in good state
      (is (= 1 (length (process-command-stack server))))
      (is (= 0 (length (process-data-stack server)))))))

;;; test 2: test some interleaved calls

(deftest test-factorial-interleaving ()
  ;; set up bare server
  (let* ((*local-courier* (make-courier))
         (simulation (make-simulation))
         (server (spawn-process 'arithmetic-server))
         (clock 0))
    (dolist (item (list *local-courier* server))
      (simulation-add-event simulation (make-event :callback item)))
    (flet ((delay-send (message time)
             (make-event :time time
                         :callback (ignorant-lambda
                                     (with-futures
                                       (send-message (process-public-address server)
                                                     message)))))
           (test-expect (x)
             (destructuring-bind (&key listen-address start-time out) x
               (receive-message (listen-address done-message)
                 (message-rpc-done
                  (is (= out (message-rpc-done-result done-message)))
                  (format t "~&Query from ~3d finished at ~3d~%"
                          start-time clock)
                  nil)
                 (otherwise
                  x)))))
      ;; set up some calls
      (let (expects)
        (loop :for (in tco? out time) :in '((10 nil   3628800  0)
                                            (10 t     3628800  5)
                                            (12 nil 479001600 10)
                                            ( 8 t       40320 18))
              :for listen-address := (register)
              :for payload := (funcall (if tco?
                                           #'make-message-factorial-tco
                                           #'make-message-factorial)
                                       :reply-channel listen-address
                                       :n in)
              :do (simulation-add-event simulation (delay-send payload time))
                  (push (list ':listen-address listen-address
                              ':start-time time
                              ':out out)
                        expects))
        ;; await results to come in
        (loop
          (incf clock)
          (simulation-run simulation :canary (canary-until clock))
          (when (endp expects)
            (return))
          (setf expects (remove-if-not #'test-expect expects)))))))
