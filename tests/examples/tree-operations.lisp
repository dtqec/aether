;;;; tests/examples/tree-operations.lisp
;;;;
;;;; To show off built-in convergecast functionality, calculates the size,
;;;; depth, and number of leaves of a distributed tree. These could be
;;;; calculated by other means, with potentially better message complexity
;;;; (depending on the network topology) but they nonetheless serve as
;;;; succinct examples.

(in-package #:aether-tests)

(defstruct (convergecast-tree-size (:include message))
  "Used to determine the size of a tree of processes.")

(defstruct (convergecast-tree-depth (:include message))
  "Used to determine the depth of a tree of processes by using `DEPTH' as a carry."
  (depth 0 :type integer))

(defstruct (convergecast-tree-depth-no-carry (:include message))
  "Used to determine the depth of a tree of processes without using a carry.")

(defstruct (convergecast-tree-leaves (:include message))
  "Used to determined the number of leaves in a tree of processes.")

(define-convergecast-handler handle-convergecast-tree-size
    ((process process-tree) (message convergecast-tree-size))
  "Computes the size of the tree of processes."
  (push-convergecast-frame :targets (process-tree-children process)
                           :func #'aether::reduce+
                           :input 1))

(define-convergecast-handler handle-convergecast-tree-depth
    ((process process-tree) (message convergecast-tree-depth))
  "Computes the depth of a tree of processes by incrementing a carry."
  (incf (convergecast-tree-depth-depth message))
  (push-convergecast-frame :targets (process-tree-children process)
                           :func #'aether::reduce-max
                           :input (convergecast-tree-depth-depth message)))

(define-convergecast-handler handle-convergecast-tree-depth-no-carry
    ((process process-tree) (message convergecast-tree-depth-no-carry))
  "Computes the depth of a tree of processes without using a carry. Demonstrates the power of `FUNCALL' over `REDUCE'."
  (flet ((1+reduce-max (input replies)
           (declare (ignore input))
           (1+ (reduce #'max replies))))
    (push-convergecast-frame :targets (process-tree-children process)
                             :func #'1+reduce-max
                             :input 1)))

(define-convergecast-handler handle-convergecast-tree-leaves
    ((process process-tree) (message convergecast-tree-leaves))
  "Computes the number of leaves of a tree of processes."
  (cond
    ((process-tree-children process)
     (push-convergecast-frame :targets (process-tree-children process)
                              :func #'aether::reduce+
                              :input 0))
    (t
     (push-convergecast-frame :targets (process-tree-children process)
                              :func #'aether::reduce+
                              :input 1))))

(define-message-dispatch process-tree
  (convergecast-tree-size            'handle-convergecast-tree-size)
  (convergecast-tree-depth           'handle-convergecast-tree-depth)
  (convergecast-tree-depth-no-carry  'handle-convergecast-tree-depth-no-carry)
  (convergecast-tree-leaves          'handle-convergecast-tree-leaves))

(deftest test-process-tree-operations ()
  "Tests that we can implement some common tree operations in a distributed setting using built-in convergecast facilities. See `ADD-TREE-PROCESSES' for the fixture used to build the tree of processes."
  (with-address-dereferencing ()
    (with-courier ()
      (with-simulation (simulation (*local-courier*))
        (let ((processes (add-tree-processes simulation))
              (rx-channel (register)))
          (loop :for message :in (list
                                  (make-convergecast-tree-size
                                   :reply-channel rx-channel)
                                  (make-convergecast-tree-depth
                                   :reply-channel rx-channel)
                                  (make-convergecast-tree-depth-no-carry
                                   :reply-channel rx-channel)
                                  (make-convergecast-tree-leaves
                                   :reply-channel rx-channel))
                :for answer :in (list 11 4 4 6)
                :for time :in (list 10 20 30 40)
                :do (send-message (process-public-address (nth 0 processes)) message)
                    (simulation-run simulation :canary (canary-until time))
                    (receive-message (rx-channel convergecast-response)
                      (message-rpc-done
                       (let ((result (message-rpc-done-result convergecast-response)))
                         (is (= result answer))))
                      (otherwise
                       (error "No result received.")))))))))
