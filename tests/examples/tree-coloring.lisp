;;;; tests/examples/tree-coloring.lisp
;;;;
;;;; To show off built-in broadcast functionality, 2-colors a tree
;;;; of processes with a single (very short) message handler.

(in-package #:aether-tests)

(defclass process-tree-coloring (process-tree)
  ((color
    :initform nil
    :type (or null bit)
    :accessor process-tree-coloring-color))
  (:documentation "A tree node that coordinates with its `CHILDREN' so as to have distinct `COLOR's."))

(defstruct (broadcast-coloring (:include message))
  "Sets the color of a process to one of two `COLOR' values."
  (color 0 :type bit))

(define-broadcast-handler handle-broadcast-coloring
    ((process process-tree-coloring) (message broadcast-coloring) now)
  "Sets this process's color according to the message, and inverts the message's color."
  (setf (process-tree-coloring-color process) (broadcast-coloring-color message))
  (setf (broadcast-coloring-color message) (- 1 (broadcast-coloring-color message)))
  (push-broadcast-frame :targets (process-tree-children process)))

(define-message-dispatch process-tree-coloring
  (broadcast-coloring 'handle-broadcast-coloring))

(deftest test-process-tree-2-coloring ()
  "Sends a message to the root of a tree of processes telling it to 2-color itself and its children, and then checks that the 2-coloring is correct. See `ADD-TREE-PROCESSES' for the fixture used to build the tree of processes."
  (with-address-dereferencing ()
    (with-courier ()
      (with-simulation (simulation (*local-courier*))
        (let ((processes (add-tree-processes simulation 'process-tree-coloring))
              (rx-channel (register)))
          (send-message (process-public-address (nth 0 processes))
                        (make-broadcast-coloring :reply-channel rx-channel))
          (simulation-run simulation :canary (canary-until 10))
          (receive-message (rx-channel broadcast-ack)
            (message-rpc-done
             (is (equal '(0 1 1 0 0 0 0 0 1 1 1)
                        (mapcar #'process-tree-coloring-color processes))))
            (otherwise
             (error "No acknowledgement received."))))))))
