;;;; tests/examples/coloring.lisp
;;;;
;;;; Randomized algorithm for 3-coloring a line that runs on average in time
;;;; logarithmic in the line length.
;;;;
;;;; A _very limited_ amount of knowledge of global network structure allows one
;;;; to solve this in constant time: the parity of the distance of a node to one
;;;; of the line's endpoints gives a 2-coloring.  Calculating (even the parity
;;;; of) this distance from local structure takes linear time, so this random-
;;;; ized algorithm represents a compromise in average complexity for minimal
;;;; network knowledge.
;;;;
;;;; Separately, since this algorithm ingests only local network structure, it's
;;;; possible to change the network structure (including _during_ execution)
;;;; while causing only local perturbation in the way the algorithm progresses.
;;;; As demonstration, we include code to add and delete nodes from the chain.

(in-package #:aether-tests)

;;;
;;; class definitions
;;;

(defclass process-coloring (process)
  ((process-clock-rate
    :initform 1)
   (workloads
    :initform 1
    :type (integer 0)
    :accessor process-coloring-workloads
    :documentation "Number of times a process should re-check that it has produced a good answer. If the network structure is stable, this need only be done once; otherwise, this is increased to counteract potentially stale information.")
   (color
    :initform 0
    :type (integer 0 2)
    :accessor process-coloring-color)
   (neighbors
    :initform nil
    :initarg :neighbors
    :type list                          ; of addresses
    :accessor process-coloring-neighbors))
  (:documentation "A worker node that coordinates with its `NEIGHBORS' so as to have distinct `COLOR's."))

;;;
;;; verb definitions
;;;

;;; the actual coloring algorithm

(defstruct (message-color-query (:include message))
  "An RPC request for a foreign node's current color value.")

(define-rpc-handler handle-message-color-query
    ((process process-coloring) (message message-color-query) now)
  "Responds with this node's current color value."
  (process-coloring-color process))

(define-process-upkeep ((process process-coloring) now) (START)
  "Coordinates with the node's `NEIGHBORS' so as to have a distinct `COLOR' value."
  (process-continuation process `(START))
  (cond
    ((not (zerop (process-coloring-workloads process)))
     (setf (process-coloring-color process) (random 3))
     (with-replies (replies)
                   (send-message-batch #'make-message-color-query
                                       (process-coloring-neighbors process))
       (unless (member (process-coloring-color process) replies)
         (decf (process-coloring-workloads process)))))
    (t
     (wake-on-network))))

;;; node addition

(defstruct (message-inject (:include message))
  "Instruct a new node to insert itself between the two indicated neighbors."
  (neighbors nil :type list))

(defstruct (message-swap-neighbor (:include message))
  "Instruct a node to replace an `OLD' neighbor for a `NEW' one."
  (old nil :type (or null address))
  (new nil :type (or null address)))

(define-rpc-handler handle-message-swap-neighbor
    ((process process-coloring) (message message-swap-neighbor) now)
  "Handles a SWAP-NEIGHBOR message."
  (let ((new (message-swap-neighbor-new message))
        (old (message-swap-neighbor-old message)))
    (setf (process-coloring-neighbors process)
          (cond
            ((null old)
             (list* new (process-coloring-neighbors process)))
            ((null new)
             (remove old (process-coloring-neighbors process)
                     :test #'address=))
            (t
             (substitute new old (process-coloring-neighbors process)
                         :test #'address=))))
    (incf (process-coloring-workloads process))))

(define-rpc-handler handle-message-inject
    ((process process-coloring) (message message-inject) now)
  "Handles an INJECT message."
  (let ((neighbors (message-inject-neighbors message))
        (address (process-public-address process)))
    (when (first neighbors)
      (send-message (first neighbors)
                    (make-message-swap-neighbor :old (second neighbors)
                                                :new address)))
    (when (second neighbors)
      (send-message (second neighbors)
                    (make-message-swap-neighbor :old (first neighbors)
                                                :new address)))
    t))

;;; node deletion

(defstruct (message-kill (:include message))
  "Instruct a node to remove itself from the line.")

(define-rpc-handler handle-message-kill
    ((process process-coloring) (message message-kill) now)
  "Given a node, tell its neighbors to sew over it, restart their coloring processes, and stop this node."
  (let ((neighbors (process-coloring-neighbors process))
        (address (process-public-address process)))
    (when (first neighbors)
      (send-message (first neighbors)
                    (make-message-swap-neighbor :old address
                                                :new (second neighbors))))
    (when (second neighbors)
      (send-message (second neighbors)
                    (make-message-swap-neighbor :old address
                                                :new (first neighbors))))
    (setf (process-coloring-workloads process) 0)
    t))

;;; generic things

(define-message-dispatch process-coloring
  (message-color-query   'handle-message-color-query)
  (message-kill          'handle-message-kill)
  (message-inject        'handle-message-inject)
  (message-swap-neighbor 'handle-message-swap-neighbor))

;;;
;;; tests
;;;

;;; test 1: does this actually solve the coloring problem?

(defun build-coloring-components (node-count nodes-per-courier)
  "Sets up the basic components of a coloring problem: a courier network, coloring nodes on that network, and a simulation housing them.  Returns a triple (COURIERS NODES SIMULATION)."
  ;; build courier network
  (let* ((courier-count (floor node-count nodes-per-courier))
         (courier-grid (make-courier-grid courier-count 1))
         (couriers (make-array courier-count :displaced-to courier-grid))
         (nodes (make-array node-count))
         (simulation (make-simulation)))
    ;; install courier callbacks
    (loop :for courier :across couriers
          :do (setf (courier-processing-clock-rate courier) 5)
              (simulation-add-event simulation (make-event :callback courier :time 0)))
    ;; build nodes within couriers
    (dotimes (j node-count)
      (let ((*local-courier* (aref couriers (floor j nodes-per-courier))))
        (setf (aref nodes j)
              (spawn-process 'process-coloring))
        (simulation-add-event simulation (make-event :callback (aref nodes j) :time 0))))
    ;; set up each node's neighbors
    (dotimes (j node-count)
      (when (<= 0 (1- j))
        (push (process-public-address (aref nodes (1- j)))
              (process-coloring-neighbors (aref nodes j))))
      (when (< (1+ j) node-count)
        (push (process-public-address (aref nodes (1+ j)))
              (process-coloring-neighbors (aref nodes j)))))
    (list couriers nodes simulation)))

(defun run-coloring-simulation (simulation nodes &optional (start-time 0) end-time (delta 1))
  "Runs `SIMULATION', beginning at `START-TIME', until either `END-TIME' is reached or all of `NODES' is stable."
  (loop
    (when (and end-time (>= start-time end-time))
      (return start-time))
    (incf start-time delta)
    (simulation-run simulation :canary (canary-until start-time))
    (when (every (alexandria:compose #'zerop #'process-coloring-workloads) nodes)
      (return start-time))))

(defun check-coloring-stability (nodes)
  "Checks that the array `NODES' carries a valid 3-coloring."
  (dotimes (j (length nodes))
    (when (<= 0 (1- j))
      (assert (/= (process-coloring-color (aref nodes j))
                  (process-coloring-color (aref nodes (1- j))))))
    (when (< (1+ j) (length nodes))
      (assert (/= (process-coloring-color (aref nodes j))
                  (process-coloring-color (aref nodes (1+ j))))))))

(defun perform-coloring-test (node-count nodes-per-courier)
  "Constructs a family of worker nodes of size `NODE-COUNT', distributed across a network with `NODES-PER-COURIER' many nodes assigned to a linear family of couriers. Then, runs the simulation until the nodes finish 3-coloring, and reports the amount of time (in simulation ticks) it took to complete."
  (destructuring-bind (couriers nodes simulation)
      (build-coloring-components node-count nodes-per-courier)
    (declare (ignore couriers))
    ;; spin up simulation, run until everyone's stopped
    (let ((time-limit (run-coloring-simulation simulation nodes)))
      ;; check coloring problem is solved
      (check-coloring-stability nodes)
      time-limit)))

(deftest test-coloring ()
  "Check that the 3-coloring algorithm produces valid 3-colorings.  Also compute some runtime statistics, so that we can see the advertised log-linear scaling."
  (dolist (j (alexandria:iota 8 :start 2))
    (let ((node-count (expt 2 j))
          (times nil))
      (dotimes (k 500)
        (push (perform-coloring-test node-count 2) times))
      ;; check all colorings are solved
      (is (every #'identity times))
      (format t "~&Solving for ~3d nodes took ~6f ticks on average (stddev ~6f)~%"
              node-count (alexandria:mean times) (alexandria:standard-deviation times)))))

;;; test 2a: add a node after solution is reached
;;; test 2b: add a node during solving

(deftest test-stable-node-injection ()
  "Check that we can add new nodes to the line after a 3-coloring has finished."
  (let* ((node-count 16)
         (nodes-per-courier 4))
    (assert (<= 4 node-count))
    (destructuring-bind (couriers nodes simulation)
        (build-coloring-components node-count nodes-per-courier)
      (let ((time-limit (run-coloring-simulation simulation nodes)))
        ;; inject a couple of nodes
        (let* ((*local-courier* (aref couriers 0))
               (middle-pos (floor node-count 2))
               (middle-node (spawn-process 'process-coloring))
               (edge-node (spawn-process 'process-coloring)))
          (simulation-add-event simulation (make-event :callback middle-node :time time-limit))
          (simulation-add-event simulation (make-event :callback edge-node :time time-limit))
          (send-message (process-public-address middle-node)
                        (make-message-inject
                         :neighbors (list (process-public-address (aref nodes middle-pos))
                                          (process-public-address (aref nodes (1- middle-pos))))))
          (send-message (process-public-address edge-node)
                        (make-message-inject
                         :neighbors (list (process-public-address (aref nodes 0)))))
          (setf nodes (concatenate 'vector
                                   (list edge-node)
                                   (subseq nodes 0 middle-pos)
                                   (list middle-node)
                                   (subseq nodes middle-pos)))
          (run-coloring-simulation simulation nodes)
          (check-coloring-stability nodes)
          (is t))))))

(deftest test-unstable-node-injection ()
  "Check that we can add new nodes to the line while a 3-coloring is being computed."
  (let* ((node-count 16)
         (nodes-per-courier 4)
         (interrupt-time 1))
    (assert (<= 4 node-count))
    (destructuring-bind (couriers nodes simulation)
        (build-coloring-components node-count nodes-per-courier)
      (let ((time-limit (run-coloring-simulation simulation nodes 0 interrupt-time)))
        ;; inject a couple of nodes
        (let* ((*local-courier* (aref couriers 0))
               (middle-pos (floor node-count 2))
               (middle-node (spawn-process 'process-coloring))
               (edge-node (spawn-process 'process-coloring)))
          ;; check that we're actually doing something unstable
          (when (or (zerop (process-coloring-workloads (aref nodes middle-pos)))
                    (zerop (process-coloring-workloads (aref nodes (1- middle-pos)))))
            ;; if it's stable, then try again
            (break)
            (return-from test-unstable-node-injection (test-unstable-node-injection)))
          (simulation-add-event simulation (make-event :callback middle-node :time time-limit))
          (simulation-add-event simulation (make-event :callback edge-node :time time-limit))
          (send-message (process-public-address middle-node)
                        (make-message-inject
                         :neighbors (list (process-public-address (aref nodes middle-pos))
                                          (process-public-address (aref nodes (1- middle-pos))))))
          (send-message (process-public-address edge-node)
                        (make-message-inject
                         :neighbors (list (process-public-address (aref nodes 0)))))
          (setf nodes (concatenate 'vector
                                   (list edge-node)
                                   (subseq nodes 0 middle-pos)
                                   (list middle-node)
                                   (subseq nodes middle-pos)))
          (run-coloring-simulation simulation nodes)
          (check-coloring-stability nodes)
          (is t))))))

;;; test 3a: delete a node after solution is reached
;;; test 3b: delete a node during solving

(deftest test-stable-node-deletion ()
  "Check that we can delete nodes from the line after a 3-coloring has finished."
  (let* ((node-count 16)
         (nodes-per-courier 4))
    (assert (<= 4 node-count))
    (destructuring-bind (couriers nodes simulation)
        (build-coloring-components node-count nodes-per-courier)
      (let ((start-time (run-coloring-simulation simulation nodes))
            (*local-courier* (aref couriers 0))
            (middle-pos (floor node-count 2)))
        ;; delete a couple of nodes
        (with-active-simulation simulation
          (send-message-batch #'make-message-kill (mapcar #'process-public-address
                                                          (list (aref nodes middle-pos)
                                                                (aref nodes 0)))))
        (setf nodes (concatenate 'vector
                                 (subseq nodes 1 middle-pos)
                                 (subseq nodes (1+ middle-pos))))
        ;; manually simulate for a bit to let the messages propagate
        (incf start-time 2)
        (simulation-run simulation :canary (canary-until start-time))
        (run-coloring-simulation simulation nodes start-time)
        (check-coloring-stability nodes)
        (is t)))))

(deftest test-unstable-node-deletion ()
  "Check that we can delete nodes from the line while a 3-coloring is still being computed."
  (let* ((node-count 16)
         (nodes-per-courier 4)
         (interrupt-time 1))
    (assert (<= 4 node-count))
    (destructuring-bind (couriers nodes simulation)
        (build-coloring-components node-count nodes-per-courier)
      (let ((start-time (run-coloring-simulation simulation nodes 0 interrupt-time))
            (*local-courier* (aref couriers 0))
            (middle-pos (floor node-count 2)))
        ;; check that we're actually doing something unstable
        (when (or (zerop (process-coloring-workloads (aref nodes middle-pos)))
                  (zerop (process-coloring-workloads (aref nodes (1- middle-pos)))))
          ;; if it's stable, then try again
          (break)
          (return-from test-unstable-node-deletion (test-unstable-node-deletion)))
        ;; delete a couple of nodes
        (with-active-simulation simulation
          (send-message-batch #'make-message-kill (mapcar #'process-public-address
                                                          (list (aref nodes middle-pos)
                                                                (aref nodes 0)))))
        (setf nodes (concatenate 'vector
                                 (subseq nodes 1 middle-pos)
                                 (subseq nodes (1+ middle-pos))))
        ;; manually simulate for a bit to let the messages propagate
        (incf start-time 2)
        (simulation-run simulation :canary (canary-until start-time))
        (run-coloring-simulation simulation nodes start-time)
        (check-coloring-stability nodes)
        (is t)))))
