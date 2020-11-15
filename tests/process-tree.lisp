;;;; tests/examples/process-tree.lisp
;;;;
;;;; Provides some boilerplate for building a distributed tree of processes.

(in-package #:aether-tests)

(defclass process-tree (process)
  ((process-clock-rate
    :initform 1)
   (id
    :initarg :id
    :accessor process-tree-id)
   (children
    :initform nil
    :initarg :children
    :type list
    :accessor process-tree-children))
  (:documentation "A `PROCESS' that is part of a tree of processes. Has an `ID' and a list of `CHILDREN'."))

(define-process-upkeep ((process process-tree) now)
    (START)
  "This process sits in an infinite loop."
  (process-continuation process `(START)))

(defun build-children (process)
  "Announces the process addresses beneath the current process according to the following tree:

   0--1-----3---8
    \  \---4 \-9
     \  \-5
      \-2---6--10
         \-7

Modified from a method definition in tests/recursive-lock.lisp.
"
  (assert (subtypep (type-of process) 'process-tree))
  (let ((children
          (case (process-tree-id process)
            (0 '(1 2))
            (1 '(3 4 5))
            (2 '(6 7))
            (3 '(8 9))
            (6 '(10))
            (otherwise nil)))
        (child-addresses nil))
    (dohash ((address process) aether::*dereferencing-table* child-addresses)
      (when (and (subtypep (type-of process) 'process-tree)
                 (member (process-tree-id process) children))
        (push address child-addresses)))))

(defun add-tree-processes (simulation &optional (process-type 'process-tree))
  "Spawns a collection of 11 `PROCESS'es of type `PROCESS-TYPE', adds them to the `SIMULATION', and installs their children using `BUILD-CHILDREN' to create a tree of processes. This is meant to be used as a test fixture, rather than being generic / extensible."
  (let ((processes
          (loop
            :for j :from 0 :below 11
            :for process := (spawn-process process-type :id j :debug? t)
            :do (simulation-add-event simulation (make-event :callback process))
            :collect process)))
    (loop :for p :in processes
          :do (setf (process-tree-children p) (build-children p)))
    processes))
