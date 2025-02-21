;;;; tests/event.lisp
;;;;
;;;; Tests which cover the functionality of the aether event loop.

(in-package #:aether-tests)

;;;
;;; simple test of raw event loop behavior
;;;

(defclass a ()
  ((slot :accessor a-slot :initform 0)))
(defclass b ()
  ((cutoff :accessor b-cutoff :initarg :cutoff)))
(defclass c (a)
  ())

(define-object-handler ((a a) now)
  (incf (a-slot a) 2)
  (log-entry :source a
             :source-type (type-of a)
             :time now
             :entry-type 'a
             :slot (a-slot a))
  (schedule a (1+ now)))

(define-object-handler ((b b) now)
  (log-entry :source b
             :source-type (type-of b)
             :time now
             :entry-type 'b)
  (when (< now (b-cutoff b))
    (schedule b (+ 1 now))
    (schedule b (+ 2 now))))

(define-object-handler ((c c) now)
  (log-entry :source c
             :source-type (type-of c)
             :time now
             :entry-type 'c)
  (call-next-method))

(deftest test-event-loop ()
  (with-transient-logger ()
    (let* ((simulation (make-simulation))
           (c (make-instance 'c))
           (b (make-instance 'b :cutoff 3)))
      (simulation-add-event simulation (make-event :callback c :time 0))
      (simulation-add-event simulation (make-event :callback b :time 1))
      (simulation-run simulation :canary (canary-until 3))
      (is (equalp (mapcar (lambda (x)
                            (ecase (getf x ':entry-type)
                              (a `(:a ,(getf x ':time) ,(getf x ':slot)))
                              (b `(:b ,(getf x ':time)))
                              (c `(:c ,(getf x ':time)))))
                          (reverse (logger-entries *logger*)))
                  '((:C 0) (:A 0 2)
                    (:B 1) (:C 1) (:A 1 4) 
                    (:B 2) (:C 2) (:A 2 6) 
                    (:B 3) (:B 3) (:C 3) (:A 3 8)))))))
