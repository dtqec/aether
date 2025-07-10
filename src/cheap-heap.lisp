;;;; cheap-heap.lisp
;;;;
;;;; Implements a variant data structure that conforms to the interface of a
;;;; priority queue but is more suitable for scenarios with extremely low unique
;;;; key counts.
;;;;
;;;; TODO: This currently uses a linked list implementation for the "toplevel"
;;;;     data structure.  It could be that we should still be using an honest
;;;;     priority queue at toplevel.

(in-package #:aether)

(defstruct (cheap-heap)
  (queues (list (cons -1 nil) nil))
  (hash (make-hash-table)))

(defun cheap-heap-enqueue (heap item key)
  ;; if we can find a write pointer, use it
  (with-slots (hash queues) heap
    (multiple-value-bind (queue found?) (gethash key hash)
      (unless found?
        ;; otherwise, need to build a fresh one
        (labels ((add-queue (queues key)
                   ;; we're not past the smaller buckets, fast forward
                   (when (and (cadr queues)
                              (> key (caadr queues)))
                     (return-from add-queue (add-queue (cdr queues) key)))
                   ;; we are past the smaller buckets.  the nonexistence of the
                   ;; write pointer guarantees that the key at our cursor is
                   ;; larger than the one we're searching for (or empty), so
                   ;; we add a fresh bucket here.
                   (setf queue (make-q)
                         (gethash key hash) queue
                         (cdr queues) (list* (cons key queue) (cdr queues)))))
          (add-queue queues key)))
      (q-enq item queue)
      (values))))

(defun cheap-heap-dequeue (heap)
  (with-slots (queues hash) heap
    ;; no more queues in heap
    (unless (cadr queues)
      (return-from cheap-heap-dequeue nil))
    ;; top queue in heap is nonempty
    (multiple-value-bind (value nonempty?) (q-deq (cdadr queues))
      (when nonempty?
        (return-from cheap-heap-dequeue value)))
    ;; top queue in heap is empty
    (remhash (cdar queues) hash)
    (setf (cdr queues) (cddr queues))
    (cheap-heap-dequeue heap)))

;; REM: cheap-heap-deep-peep also ripe for implementation
(defun cheap-heap-peep (heap)
  (with-slots (queues) heap
    (cond
      ;; there is a top queue
      ((and (cadr queues)
            (q-peek (cdadr queues)))
       (values (q-peek (cdadr queues)) t))
      ;; there is a second queue
      ;; TODO: i think this is wrong is general?? only fine for simulations which don't rewind.
      ((and (caddr queues)
            (q-peek (cdaddr queues)))
       (values (q-peek (cdaddr queues)) t))
      ;; there isn't a top queue
      (t
       (values nil nil)))))

(defun cheap-heap-empty? (heap)
  (loop :for (_key . q) :in (rest (cheap-heap-queues heap))
        :always (or (null q) (q-empty q))))

(defun cheap-heap->list (heap)
  (labels ((aux (heap &optional (list nil))
             (let ((next (cheap-heap-dequeue heap)))
               (cond
                 ((null next)
                  (reverse list))
                 (t
                  (aux heap (list* next list)))))))
    (aux heap)))
