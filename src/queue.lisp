;;;; queue.lisp
;;;;
;;;; Minimalist implementation of a queue, based off of an implementation in
;;;; CL-QUIL by Corwin de Boor, but probably not original to him.

(in-package #:aether)

;; The internal structure of a queue looks like ((NIL . (ITEMS ... . A)) A),
;; where A is typically a single cons cell (NIL . NIL).
;; The common cell A is used to detect when the queue is exhausted, and the
;; prefixed NIL is used to make mid-queue-deletion somewhat easier.
(defun make-q (&rest items)
  (let ((dummy-tail (list nil)))
    (list* (nconc (list* nil items) dummy-tail) dummy-tail)))

(defun q-empty (q)
  (eql (cdar q) (cdr q)))

(defun q-enq (el q)
  (setf (cadr q) el
        (cddr q) (list nil)
        (cdr q) (cddr q))
  q)

(defun q-deq (q)
  (values (if (q-empty q) nil (pop (cdar q)))
          q))

(defun q-peek (q)
  (if (q-empty q) nil (cadar q)))

(defun q-len (q)
  (1- (length (cdar q))))

(defun q-deq-first (q pred)
  "Dequeue the first message in `Q' that satisfies `PRED'."
  (declare (optimize (speed 3) (safety 0)))
  (check-type pred function)
  (labels ((aux (q pred)
             (cond
               ((eq (cdar q) (cdr q))
                nil)
               ((funcall pred (cadar q))
                (let ((item (cadar q)))
                  (setf (cdar q) (cddar q))
                  (values item t)))
               ;; recurse
               (t
                (q-deq-first (list* (cdar q) (cdr q)) pred)))))
    (aux q pred)))

(defun q-deq-when (q pred)
  "Dequeue the message at the front of `Q' when `PRED' is T."
  (labels ((aux (q pred)
             (let ((item (q-peek q)))
               (cond
                 ((null item)
                  nil)
                 ((funcall pred item)
                  (setf (cdar q) (cddar q))
                  (values item t))
                 ;; don't recurse
                 (t
                  nil)))))
    (aux q pred)))

;; XXX: this argument order is backward from that of q-enq. pick one.
(defun q-push (q el)
  (push el (cdar q))
  q)

(defmacro doq ((var q &optional return-form) &body body)
  "Like DOLIST, but for queues."
  (a:with-gensyms (sigil rest)
    (a:once-only ((q q))
      `(block nil
         (let ((,sigil (cdr ,q)))
           (loop :for ,var :in (cdar ,q)
                 :for ,rest :on (cdar ,q)
                 :when (eq ,rest ,sigil)
                   :return ,return-form
                 :do ,@body))))))
