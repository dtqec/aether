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

(defun make-cheap-heap ()
  (list (cons -1 nil) nil))

(defun cheap-heap-enqueue (heap item key)
  ;; fast-forward 'til we're at the end of the buckets "behind" this key
  (when (and (cadr heap)
             (> key (caadr heap)))
    (return-from cheap-heap-enqueue
      (cheap-heap-enqueue (cdr heap) item key)))
  ;; get a queue for this key
  (let (queue)
    ;; at this point HEAP looks like (_ (key' . queue) ...) or (_ nil).
    ;; if key' exists and is equal to key, we use queue.
    ;; otherwise, we inject a new (key . queue) pair here.
    (cond
      ((and (cadr heap)
            (= (caadr heap) key))
       (setf queue (cdadr heap)))
      (t
       (setf queue (make-q)
             (cdr heap) (list* (cons key queue) (cdr heap)))))
    ;; finally, enqueue the actual item into this queue
    (q-enq item queue)
    (values)))

(defun cheap-heap-dequeue (heap)
  (unless (cadr heap)
    (return-from cheap-heap-dequeue nil))
  (when (q-peek (cdadr heap))
    (return-from cheap-heap-dequeue
      (q-deq (cdadr heap))))
  (setf (cdr heap) (cddr heap))
  (cheap-heap-dequeue heap))

;; REM: cheap-heap-deep-peep also ripe for implementation
(defun cheap-heap-peep (heap)
  (cond
    ((and (cadr heap)
          (q-peek (cdadr heap)))
     (q-peek (cdadr heap)))
    ((and (caddr heap)
          (q-peek (cdaddr heap)))
     (q-peek (cdaddr heap)))
    (t
     nil)))

(defun cheap-heap-empty? (heap)
  (equalp heap (make-cheap-heap)))

(defun cheap-heap->list (heap)
  (labels ((aux (heap &optional (list nil))
             (let ((next (cheap-heap-dequeue heap)))
               (cond
                 ((null next)
                  (reverse list))
                 (t
                  (aux heap (list* next list)))))))
    (aux heap)))
