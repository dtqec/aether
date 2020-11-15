;;;; tests/cheap-heap.lisp
;;;;
;;;; Tests which cover the CHEAP-HEAP implementation.

(in-package #:aether-tests)

(deftest test-cheap-heap ()
  (let* ((heap (make-cheap-heap))
         (pairs '((a 1)
                  (b 2)
                  (c 3/2)
                  (d 1)
                  (e 3/2)))
         (sorted-pairs (sort (copy-seq pairs) #'< :key #'second)))
    (dolist (pair pairs)
      (apply #'cheap-heap-enqueue heap pair))
    (loop :for next := (cheap-heap-dequeue heap)
          :for (sorted-next _) :in sorted-pairs
          :do (is (eql sorted-next next)))
    (is (null (cheap-heap-peep heap)))))
