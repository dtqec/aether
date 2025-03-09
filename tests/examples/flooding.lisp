;;;; tests/examples/flooding.lisp
;;;;
;;;; Simple broadcast-based algorithm for computing a rooted spanning tree on a
;;;; distributed network of nodes.
;;;;
;;;; See Chapter 3 in James Aspnes' notes for the Yale course CPSC 465/565
;;;; Theory of Distributed Systems (https://arxiv.org/abs/2001.04235).

(in-package #:aether-tests)

;;;
;;; class definitions
;;;

(defclass process-flooding (process)
  ((process-clock-rate
    :initform 1)
   (root
    :initform nil
    :initarg :root
    :type boolean
    :accessor process-flooding-root
    :documentation "If T, this node is to be the root of the spanning tree.")
   (parent
    :initform nil
    :type (or null address)
    :accessor process-flooding-parent
    :documentation "Once the spanning tree has been computed, points to this node's parent in the tree.")
   (children
    :initform nil
    :type list
    :accessor process-flooding-children
    :documentation "Once the spanning tree has been computed, contains a `LIST' of this node's children in the tree.")
   (neighbors
    :initform nil
    :initarg :neighbors
    :type list
    :accessor process-flooding-neighbors
    :documentation "A `LIST' of addresses of neighbors `PROCESS'es."))
  (:documentation "A node that coordinates with its `NEIGHBORS' to compute a spanning tree."))

;;;
;;; message definitions
;;;

(defstruct (message-flood (:include message))
  "Used to set the `PARENT' of the target to `PARENT-ADDRESS' if unset."
  (parent-address nil :type address))

;;;
;;; handler definitions
;;;

(define-message-handler handle-message-flood
    ((process process-flooding) (message message-flood))
  "Handles a `FLOOD' message. If the `PROCESS's parent is unset, sets it to the `PARENT-ADDRESS' and forwards along the broadcast by pushing the `BROADCAST-FLOOD' command onto the stack. Otherwise, lets the `REPLY-CHANNEL' know that we already have a parent."
  (with-slots (parent-address reply-channel) message
    (let ((parent (process-flooding-parent process)))
      (cond
        (parent
         #+i(format t "~%~A: ~A already has a parent ~A" (now) process parent)
         (send-message reply-channel (make-message-rpc-done :result nil)))
        (t
         #+i(format t "~%~A: ~A setting parent ~A" (now) process parent-address)
         (setf (process-flooding-parent process) parent-address)
         (process-continuation process `(BROADCAST-FLOOD ,reply-channel)))))))

;;;
;;; message dispatch
;;;

(define-message-dispatch process-flooding
  (message-flood  'handle-message-flood))

;;;
;;; process upkeep
;;;

(define-process-upkeep ((process process-flooding)) (START)
  "If root, start the algorithm. Otherwise loop."
  (cond
    ((process-flooding-root process)
     (setf (process-flooding-parent process) (process-public-address process))
     (process-continuation process `(BROADCAST-FLOOD ,nil)))
    (t
     (process-continuation process `(START)))))

(define-process-upkeep ((process process-flooding)) (BROADCAST-FLOOD reply-channel)
  "Broadcasts a flood message to all neighbors. For each neighbor, if it replies with result T, add it to our list of children. Finally, if we have a `REPLY-CHANNEL', let it know we're done; otherwise `HALT'."
  (let ((my-address (process-public-address process))
        (neighbors (process-flooding-neighbors process)))
    (flet ((payload-constructor ()
             (make-message-flood :parent-address my-address)))
      (with-replies (replies)
                    (send-message-batch #'payload-constructor neighbors)
        (loop :for reply :in replies
              :for neighbor :in neighbors
              :when reply
                :do (push neighbor (process-flooding-children process)))
        (when reply-channel
          (send-message reply-channel (make-message-rpc-done :result t)))))))

;;;
;;; test for spanning tree construction
;;;

(defun ids-within-1 (id ids)
  (let ((ids-within))
    (dolist (i ids ids-within)
      (let ((distance (+ (abs (- (car i) (car id)))
                         (abs (- (cadr i) (cadr id))))))
        (when (= distance 1)
          (push i ids-within))))))

(defun count-children (parent node-counter)
  (dolist (child (process-flooding-children parent))
    (incf (gethash child node-counter))))

(deftest test-flooding-spanning-tree ()
  "Builds a lattice network of nodes where two nodes are neighbors if they are one unit apart from each other. Designates the origin node as root, and runs a simulation until the root halts. Finally, checks to see if we have in fact created a spanning tree."
  (let* ((couriers (make-courier-grid 1 1))
         (*local-courier* (aref couriers 0 0))
         (simulation (make-simulation))
         (ids '((0 0) (1 0) (3 0)
                (0 1) (1 1) (2 1) (3 1)
                (1 2) (2 2) (3 2)
                (1 3) (3 3)))
         (root (spawn-process 'process-flooding
                              :root T))
         (nodes (loop :repeat (1- (length ids))
                      :collect (spawn-process 'process-flooding)))
         (table (alexandria:alist-hash-table (pairlis ids
                                                      (list* root nodes)))))
    ;; fill in neighbors
    (flet ((get-address-from-table (key)
             (process-public-address (gethash key table))))
      (dohash ((id node) table)
        (setf (process-flooding-neighbors node)
              (mapcar #'get-address-from-table (ids-within-1 id ids)))))
    ;; add everyone to the simulation
    (loop :for item :in (list* *local-courier* root nodes)
          :do (simulation-add-event simulation (make-event :callback item)))
    ;; run until the root dies
    (simulation-run simulation :canary (canary-process root))
    ;; test to make sure that we have a spanning tree by checking
    ;; that all nodes show up exactly once as someone's child
    (let ((child-counter (make-hash-table :hash-function #'hash-address
                                          :test #'address=))
          (node-addresses (mapcar #'process-public-address nodes)))
      ;; initialize child-counter hash table to have all zero values
      (dolist (addr node-addresses) (setf (gethash addr child-counter) 0))
      ;; tally up the children of every node in the simulation
      (dolist (node (list* root nodes)) (count-children node child-counter))
      #+i(dohash ((addr count) child-counter) (format t "~%~A: ~A" addr count))
      ;; verify that all the counts are equal to one
      (dolist (count (alexandria:hash-table-values child-counter))
        (is (= 1 count))))))
