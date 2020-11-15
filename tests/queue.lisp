;;;; tests/queue.lisp
;;;;
;;;; Copied from the CL-QUIL test suite.

(in-package #:aether-tests)

(deftest test-queue ()
  (let ((q (make-q)))
    (is (q-empty q))
    (q-enq 1 q)
    (is (not (q-empty q)))
    (q-enq 2 q)
    (q-enq 3 q)
    (is (= 1 (q-deq q)))
    (is (= 2 (q-deq q)))
    (is (not (q-empty q)))
    (q-enq 4 q)
    (q-enq 5 q)
    (is (= 3 (q-deq q)))
    (is (= 4 (q-deq q)))
    (is (= 5 (q-deq q)))
    (is (q-empty q))))

(deftest test-queue-make ()
  (let ((q (make-q 1 2 3 4)))
    (is (not (q-empty q)))
    (is (= 1 (q-deq q)))
    (is (= 2 (q-deq q)))
    (is (= 3 (q-deq q)))
    (q-enq 5 q)
    (is (= 4 (q-deq q)))
    (is (= 5 (q-deq q)))
    (is (q-empty q))))
