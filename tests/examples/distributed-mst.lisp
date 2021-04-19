;;;; tests/distributed-mst.lisp
;;;;
;;;; To show off some of the features of the standard library, this file
;;;; implements the distributed algorithm by Gallager et al. for computing
;;;; computing minimum-weight spanning trees (MSTs), which won the Dijkstra
;;;; Prize in 2004.
;;;;
;;;; Gallager, Robert G., Pierre A. Humblet, and Philip M. Spira.
;;;; "A distributed algorithm for minimum-weight spanning trees."
;;;; ACM Transactions on Programming Languages and systems (TOPLAS)
;;;; 5.1 (1983): 66-77.

(in-package #:aether-tests)

(deftype edge-state () '(member :BASIC :BRANCH :REJECTED))

(defstruct fragment-edge-data
  ;; denoted by SE in the paper pseudocode
  (edge-state  ':BASIC :type edge-state)
  ;; denoted by w(j) in the paper pseudocode, where j is an edge identifier
  ;; we are using the target address of an edge as its identifier
  (edge-weight 0       :type integer))

(defun find-specific-edges (adjacent-edges &key desired-state not-edge)
  "Given a `HASH-TABLE' of `ADJACENT-EDGES', return a `LIST' of `ADDRESSES' corresponding to the edges in the table that are of `DESIRED-STATE' and not equal to `NOT-EDGE' (if it is set)."
  (initialize-and-return (edges)
    (dohash ((edge edge-data) adjacent-edges edges)
      (with-slots (edge-state) edge-data
        (when (and (eql desired-state edge-state)
                   (or (null not-edge)
                       (not (address= edge not-edge))))
          (push edge edges))))))

(defun find-minimum-weight-edge (adjacent-edges &key desired-state)
  "Given a `HASH-TABLE' of `ADJACENT-EDGES', return the minimum-weight edge in the table. If `DESIRED-STATE' is set, then the edge must be of that state to be considered."
  (let (minimum-weight-edge
        (minimum-weight most-positive-fixnum))
    (dohash ((edge edge-data) adjacent-edges minimum-weight-edge)
      (with-slots (edge-state edge-weight) edge-data
        (when (and (< edge-weight minimum-weight)
                   (or (null desired-state)
                       (eql desired-state edge-state)))
          (setf minimum-weight-edge edge
                minimum-weight      edge-weight))))))

;;;
;;; class definitions
;;;

(deftype node-state () '(member :FIND :FOUND :SLEEPING))

(defclass fragment-node (process)
  ((process-clock-rate
    :initform 1)
   ;; NOTE: this means we process zero or one messages per clock tick
   (process-exhaust-inbox?
    :initform nil)
   ;; NOTE: turning this off forces us to treat the inbox like a queue
   (process-peruse-inbox?
    :initform nil)
   (id
    :initarg :id
    :type integer
    :accessor fragment-node-id
    :documentation "Used in testing.")
   ;; denoted by SN in the paper pseudocode
   (node-state
    :initform ':SLEEPING
    :type node-state
    :accessor fragment-node-state
    :documentation "Internal state of the node.")
   ;; denoted by FN in the paper pseudocode
   (fragment-weight
    :initform nil
    :type (or null integer)
    :accessor fragment-node-fragment-weight)
   ;; denoted by LN in the paper pseudocode
   (fragment-level
    :initform 0
    :type (integer 0)
    :accessor fragment-node-fragment-level
    :documentation "An integer between 0 and log N (where N is the number of nodes).")
   ;; contains edge identifiers, weights, and states (SE)
   (adjacent-edges
    :initarg :adjacent-edges
    :type hash-table
    :accessor fragment-node-adjacent-edges
    :documentation "A `HASH-TABLE' mapping `ADDRESS'es of neighboring `PROCESS'es to `FRAGMENT-EDGE-DATA' structures. This mapping encodes all the information needed to describe an edge.")
   ;; the next few slot names match the paper pseudocode
   (best-edge
    :initform nil
    :type (or null address)
    :accessor fragment-node-best-edge)
   (best-weight
    :initform nil
    :type (or null integer)
    :accessor fragment-node-best-weight)
   (find-count
    :initform nil
    :type (or null integer)
    :accessor fragment-node-find-count)
   (in-branch
    :initform nil
    :type (or null address)
    :accessor fragment-node-in-branch)
   (test-edge
    :initform nil
    :type (or null address)
    :accessor fragment-node-test-edge))
  (:documentation "A node that coordinates with its neighbors to compute a minimum-weight spanning tree (MST)."))

;;;
;;; message definitions
;;;

(defstruct (fragment-message (:include message))
  "Base structure for messages used by the MST algorithm."
  (edge nil :type (or null address)))

(defstruct (msg-connect (:include fragment-message))
  (level nil :type (or null integer)))

;; TODO: this could be reformulated as a broadcast
(defstruct (msg-initiate (:include fragment-message))
  (level  nil :type (or null integer))
  (state  nil :type (or null node-state))
  (weight nil :type (or null integer)))

(defstruct (msg-test (:include fragment-message))
  (level  nil :type (or null integer))
  (weight nil :type (or null integer)))

(defstruct (msg-accept (:include fragment-message)))

(defstruct (msg-reject (:include fragment-message)))

(defstruct (msg-report (:include fragment-message))
  (weight nil :type (or null integer)))

(defstruct (msg-change-root (:include fragment-message)))

;;;
;;; handler definitions
;;;

;; section 3 of the paper pseudocode: response to Connect
(define-message-handler handle-msg-connect
    ((node fragment-node) (message msg-connect) now)
  "If we get a Connect message from a lower-level fragment, then mark that edge as a Branch and send an Initate message to absorb that fragment. Else, if the sender is at the end of a Basic edge, then wait (via a self-send). Otherwise, we've received a Connect message from a fragment of equal level, and thus we would like to merge with it. We do so by sending it an Initiate message with a higher level, and with the edge weight.

NOTE: the following line is implemented using a guard in `DEFINE-MESSAGE-DISPATCH'
if SN = sleeping then execute procedure wakeup"
  (let ((address (process-public-address node)))
    ;; extract LN, FN, SN from node
    (with-slots (fragment-level fragment-weight node-state) node
      ;; extract j and L from message
      (with-slots (edge level) message
        ;; contains w(j) and SE(j)
        (with-slots (edge-state edge-weight)
            (gethash edge (slot-value node 'adjacent-edges))
          (log-entry :entry-type 'connect-handler
                     :edge edge
                     :edge-state edge-state
                     :edge-weight edge-weight
                     :find-count (slot-value node 'find-count)
                     :fragment-level fragment-level
                     :fragment-weight fragment-weight
                     :level level
                     :node-state node-state)
          (cond
            ;; if L < LN
            ((< level fragment-level)
             (log-entry :entry-type 'level<fragment-level)
             ;; 1. SE(j) <- Branch
             (setf edge-state ':BRANCH)
             ;; 2. send Initiate(LN, FN, SN) along j
             (send-message edge (make-msg-initiate
                                 :edge address
                                 :level fragment-level
                                 :state node-state
                                 :weight fragment-weight))
             ;; 3. if SN = Find, increment `FIND-COUNT'
             (when (eql node-state ':FIND)
               (incf (slot-value node 'find-count))))
            ;; elif SE(j) = Basic
            ((eql edge-state ':BASIC)
             (log-entry :entry-type 'edge-state=basic)
             ;; 1. place received message on end of queue
             ;; NOTE: this is implemented by self-sending the message
             (log-entry :entry-type 'queueing-message
                        :message message)
             (send-message (process-public-address node) message))
            ;; else
            (t
             (log-entry :entry-type 'else)
             ;; 1. send Initiate(LN + 1, w(j), Find) along j
             (send-message edge (make-msg-initiate
                                 :edge address
                                 :level (1+ fragment-level)
                                 :state ':FIND
                                 :weight edge-weight)))))))))

;; section 4 of the paper pseudocode: response to Initiate
(define-message-handler handle-msg-initiate
    ((node fragment-node) (message msg-initiate) now)
  "An Initiate is a broadcast that is triggered in response to handling a Connect. In step 1, we update our internal state. In step 2, we continue the broadcast if we have any branches. Finally, in step 3, we push a `TEST' onto the stack if we are in the Find state."
  (let ((address (process-public-address node)))
    (with-slots (adjacent-edges find-count) node
      (with-slots (edge level state weight) message
        (log-entry :entry-type 'initiate-handler
                   :edge edge
                   :level level
                   :state state
                   :weight weight)
        ;; 1. LN <-L; FN <- F; SN <- S; in-branch <- j;
        ;;    best-edge <- nil; best-wt <- inf
        (setf (slot-value node 'best-edge)       nil
              (slot-value node 'best-weight)     most-positive-fixnum
              (slot-value node 'fragment-level)  level
              (slot-value node 'fragment-weight) weight
              (slot-value node 'in-branch)       edge
              (slot-value node 'node-state)      state)
        ;; 2. for all i =/= j s.t. SE(i) = Branch
        (let ((edges (find-specific-edges adjacent-edges
                                          :desired-state ':BRANCH
                                          :not-edge edge)))
          (log-entry :other-branches edges)
          ;; 2a. send Initiate(L, F, S) on edge i
          ;; NOTE: need to replace the value of `EDGE' to indicate sender
          (flet ((payload-constructor ()
                   (make-msg-initiate :edge address
                                      :level level
                                      :state state
                                      :weight weight)))
            (send-message-batch #'payload-constructor edges))
          ;; 2b. if S = Find then find-count <- find-count + 1
          ;; NOTE: because we're batch sending instead of looping, we loop here
          (loop :repeat (length edges) :when (eql state ':FIND)
                :do (incf find-count)))
        ;; 3. if S = Find then execute procedure test
        (when (eql state ':FIND)
          (process-continuation node `(TEST)))))))

;; section 6 of the paper pseudocode: response to Test
(define-message-handler handle-msg-test
    ((node fragment-node) (message msg-test) now)
  "Response to a Test message. If our level is <= to that of the message, then we wait (by self-sending the message). Else, if we are part of a different fragment, then we send an Accept.

Otherwise, we should reject the edge that this message came from. We only mark the edge as Rejected if it is Basic -- if it's not Basic, then this is essentially a stale Test. However, even in the case that it is stale, we likely want to send back a Reject message to allow the sender to resume progress. Finally, we push a `TEST' onto the stack.

NOTE: the following line is implemented using a guard in `DEFINE-MESSAGE-DISPATCH'
if SN = sleeping then execute procedure wakeup"
  (let ((address (process-public-address node)))
    ;; extract LN, FN, and test-edge from node
    (with-slots (adjacent-edges test-edge) node
      ;; extract j, L, and F from message
      (with-slots (edge level weight) message
        ;; extract SE(j)
        (with-slots (edge-state) (gethash edge adjacent-edges)
          (log-entry :entry-type 'test-handler
                     :edge edge
                     :edge-state edge-state
                     :fragment-level (slot-value node 'fragment-level)
                     :fragment-weight (slot-value node 'fragment-weight)
                     :level level
                     :test-edge test-edge
                     :weight weight)
          (cond
            ;; if L > LN
            ((> level (slot-value node 'fragment-level))
             (log-entry :entry-type 'level>fragment-level)
             ;; 1. place received message on end of queue
             ;; NOTE: this is implemented by self-sending the message
             (send-message (process-public-address node) message))
            ;; else if F =/= FN
            ((/= weight (slot-value node 'fragment-weight))
             (log-entry :entry-type 'weight=/=fragment-weight)
             ;; 1. send Accept along j
             (send-message edge (make-msg-accept :edge address)))
            ;; else
            (t
             (log-entry :entry-type 'else)
             ;; 1. if SE(j) = Basic then SE(j) <- Rejected
             (when (eql edge-state ':BASIC)
               (setf edge-state ':REJECTED))
             (cond
               ;; 2. if test-edge =/= j, send Reject along j
               ;; NOTE: the null check differs from the pseudocode, but from context
               ;;       it seems that they intend for nil to be considered not equal
               ((or (null test-edge) (not (address= test-edge edge)))
                (log-entry :entry-type 'test-edge=/=edge)
                (send-message edge (make-msg-reject :edge address)))
               ;; 2. else, execute procedure test
               (t
                (process-continuation node `(TEST)))))))))))

;; section 7 of the paper pseudocode: response to Accept
(define-message-handler handle-msg-accept
    ((node fragment-node) (message msg-accept) now)
  "If accepted edge weight is less than `BEST-WEIGHT', update `BEST-WEIGHT' and `BEST-EDGE'. Either way, push `REPORT' onto the stack."
  ;; extract best-edge, best-wt, test-edge from node
  (with-slots (adjacent-edges best-edge best-weight test-edge) node
    ;; extract j from message
    (with-slots (edge) message
      ;; extract w(j)
      (with-slots (edge-weight) (gethash edge adjacent-edges)
        ;; 1. test-edge <- nil
        (setf test-edge nil)
        ;; 2. if w(j) < best-wt
        (when (< edge-weight best-weight)
          ;; then best-edge <- j, best-wt <- w(j)
          (setf best-edge edge best-weight edge-weight))
        ;; 3. execute procedure report
        (process-continuation node `(REPORT))))))

;; section 8 of the paper pseudocode: response to Reject
(define-message-handler handle-msg-reject
    ((node fragment-node) (message msg-reject) now)
  "Mark edge as Rejected (if it is Basic), and push `TEST' onto the stack."
  (with-slots (adjacent-edges) node
    ;; extract j from message
    (with-slots (edge) message
      ;; extract SE(j)
      (with-slots (edge-state) (gethash edge adjacent-edges)
        ;; 1. if SE(j) = Basic then SE(j) <- Rejected
        (when (eql edge-state ':BASIC) (setf edge-state ':REJECTED))
        ;; 2. execute procedure test
        (process-continuation node `(TEST))))))

;; section 10 of the paper pseudocode: response to Report
(define-message-handler handle-msg-report
    ((node fragment-node) (message msg-report) now)
  "If we get a Report from someone that's not our `IN-BRANCH', then we decrement `FIND-COUNT', potentially update our best edge & weight, and then proceed to `REPORT'. Else, if we're in the Find state, wait (via a self-send). Else, if the Report weight is lower than `BEST-WEIGHT', we should `CHANGE-ROOT'. Finally, if the Report weight and `BEST-WEIGHT' are both inifinity, then we're part of the core and have discovered the full MST, so it's time to stop."
  ;; extract best-edge, best-wt, find-count, in-branch, SN from node
  (with-slots (best-edge best-weight find-count in-branch) node
    ;; extract j and w from message
    (with-slots (edge weight) message
      (log-entry :entry-type 'report-handler
                 :best-edge best-edge
                 :best-weight best-weight
                 :edge edge
                 :find-count find-count
                 :in-branch in-branch
                 :node-state (slot-value node 'node-state)
                 :weight weight)
      (cond
        ;; if j =/= in-branch
        ((not (address= edge in-branch))
         (log-entry :entry-type 'j=/=in-branch)
         ;; 1. find-count <- find-count - 1
         (decf find-count)
         ;; 2. if w < best-wt then best-weight <- w; best-edge <- j
         (when (< weight best-weight)
           (setf best-weight weight best-edge edge))
         ;; 3. execute procedure report
         (process-continuation node `(REPORT)))
        ;; elif SN = Find
        ((eql (slot-value node 'node-state) ':FIND)
         (log-entry :entry-type 'node-state=find)
         ;; 1. place received message on end of queue
         ;; NOTE: this is implemented by self-sending the message
         (log-entry :entry-type 'queueing-message
                    :message message)
         (send-message (process-public-address node) message))
        ;; elif w > best-wt
        ((> weight best-weight)
         (log-entry :entry-type 'weight>best-weight)
         ;; 1. execute procedure change-root
         (process-continuation node `(CHANGE-ROOT)))
        ;; else if w = best-wt = inf
        ((and (= weight best-weight) (= weight most-positive-fixnum))
         (log-entry :entry-type 'weight=best-weight=infinity)
         ;; 1. halt operation
         (process-continuation node `(HALT)))))))

;; section 12 of the paper pseudocode: response to Change-root
(define-message-handler handle-msg-change-root
    ((node fragment-node) (message msg-change-root) now)
  "Push a `CHANGE-ROOT' command onto the stack."
  (process-continuation node `(CHANGE-ROOT)))

;;;
;;; message dispatch
;;;

;; NOTE: the guards are used to force nodes to wake up before handling messages
(define-message-dispatch fragment-node
  (msg-connect      'handle-msg-connect
                    (not (eql (fragment-node-state fragment-node)
                              ':SLEEPING)))
  (msg-initiate     'handle-msg-initiate)
  (msg-test         'handle-msg-test
                    (not (eql (fragment-node-state fragment-node)
                              ':SLEEPING)))
  (msg-accept       'handle-msg-accept)
  (msg-reject       'handle-msg-reject)
  (msg-report       'handle-msg-report)
  (msg-change-root  'handle-msg-change-root))

;;;
;;; process upkeep
;;;

(define-process-upkeep ((node fragment-node) now) (HALT)
  "Empty the command stack and kill the process."
  (setf (process-command-stack node) '())
  (process-die))

;; section 1 of the paper pseudocode: response to awakening
(define-process-upkeep ((node fragment-node) now) (START)
  "If sleeping, wake up. Otherwise loop forever."
  (process-continuation node `(START))
  ;; if asleep, push the wakeup procedure onto the stack
  (when (eql (slot-value node 'node-state) ':SLEEPING)
    (process-continuation node `(WAKEUP))))

;; section 2 of the paper pseudocode: wakeup procedure
(define-process-upkeep ((node fragment-node) now) (WAKEUP)
  "Wake up, by finding the minimum-weight adjacent edge, initializing local state, and sending our initial Connect message."
  (let ((address (process-public-address node)))
    (with-slots (adjacent-edges find-count fragment-level) node
      ;; double-check that we're sleeping
      (when (eql (slot-value node 'node-state) ':SLEEPING)
        ;; 1. find minimum-weight adjacent edge m
        (let ((minimum-weight-edge
                (find-minimum-weight-edge adjacent-edges)))
          (with-slots (edge-state)
              (gethash minimum-weight-edge adjacent-edges)
            (log-entry :entry-type 'wakeup
                       :minimum-weight-edge minimum-weight-edge
                       :edge-state edge-state)
            ;; 2. SET(m) <- Branch; LN <- 0; SN <- Found; find-count <- 0
            (setf edge-state                     ':BRANCH
                  fragment-level                 0
                  (slot-value node 'node-state)  ':FOUND
                  find-count                     0)
            ;; 3. send Connect(0) along m
            (send-message minimum-weight-edge
                          (make-msg-connect :edge address
                                            :level 0))))))))

;; section 5 of the paper pseudocode: test procedure
;; NOTE: this is the only procedure that calls another procedure
;;       watch out for this as it's unclear if that matters
(define-process-upkeep ((node fragment-node) now) (TEST)
  "If we have an adjacent edge in the `:BASIC' state, then send a Test message along that edge. Otherwise, push a `REPORT' command onto the stack."
  (with-slots (adjacent-edges test-edge) node
    ;; find minimum-weight adjacent edge in Basic state, if exists
    (let ((minimum-weight-edge
            (find-minimum-weight-edge adjacent-edges
                                      :desired-state ':BASIC)))
      (cond
        ;; if exists
        (minimum-weight-edge
         (log-entry :entry-type 'found-minimum-weight-basic-edge
                    :edge minimum-weight-edge)
         ;; 1. test-edge <- minimum-weight-edge
         (setf test-edge minimum-weight-edge)
         ;; 2. send Test(LN, FN) on test-edge
         (send-message test-edge
                       (make-msg-test
                        :edge (process-public-address node)
                        :level (slot-value node 'fragment-level)
                        :weight (slot-value node 'fragment-weight))))
        ;; else
        (t
         (log-entry :entry-type 'else)
         ;; 1. test-edge <- nil
         (setf test-edge nil)
         ;; 2. execute procedure report
         (process-continuation node `(REPORT)))))))

;; section 9 of the paper pseudocode: report procedure
(define-process-upkeep ((node fragment-node) now) (REPORT)
  "When `FIND-COUNT' is zero and `TEST-EDGE' is NIL, set state to `:FOUND' and report our `BEST-WEIGHT' to `IN-BRANCH'."
  (with-slots (best-weight find-count in-branch test-edge) node
    ;; if find-count = 0 and test-edge = nil
    (log-entry :entry-type 'report
               :find-count find-count
               :test-edge test-edge)
    (when (and (zerop find-count) (null test-edge))
      ;; 1. SN <- Found
      (setf (slot-value node 'node-state) ':FOUND)
      ;; 2. send Report(best-wt) on in-branch
      (send-message in-branch (make-msg-report
                               :edge (process-public-address node)
                               :weight best-weight)))))

;; section 11 of the paper pseudocode: change-core procedure
;; TODO: probably rename this and the accompanying message to change-core
(define-process-upkeep ((node fragment-node) now) (CHANGE-ROOT)
  "If the `BEST-EDGE' is a `:BRANCH', send a `MSG-CHANGE-ROOT' along it. Otherwise, send a `MSG-CONNECT' along it with our current `FRAGMENT-LEVEL'."
  (let ((address (process-public-address node)))
    ;; extract best-edge, LN from node
    (with-slots (adjacent-edges best-edge fragment-level) node
      ;; extract SE(best-edge)
      (with-slots (edge-state) (gethash best-edge adjacent-edges)
        (cond
          ;; if SE(best-edge) = Branch
          ((eql edge-state ':BRANCH)
           ;; 1. send Change-root on best-edge
           (send-message best-edge
                         (make-msg-change-root :edge address)))
          ;; else
          (t
           ;; 1. send Connect(LN) on best-edge
           (send-message best-edge
                         (make-msg-connect :edge address
                                           :level fragment-level))
           ;; 2. SE(best-edge) <- Branch
           (setf edge-state ':BRANCH)))))))

;;;
;;; test helper functions
;;;

(defun add-fragment-nodes (simulation fragment-edges)
  "Add `FRAGMENT-NODE's to `SIMULATION' as prescribed by the length of the ALIST `FRAGMENT-EDGES', and then call `ADD-FRAGMENT-EDGES'. Finally, return a LIST of the nodes."
  (initialize-and-return
      ((nodes
        (loop
          :for j :from 0 :below (length fragment-edges)
          :for node := (spawn-process 'fragment-node :id j :debug? t)
          :for adjacent-edges := (make-hash-table :hash-function #'hash-address
                                                  :test #'address=)
          :do (setf (fragment-node-adjacent-edges node) adjacent-edges)
              (simulation-add-event simulation (make-event :callback node))
          :collect node)))
    (add-fragment-edges nodes fragment-edges)))

(defun add-fragment-edges (nodes fragment-edges)
  "Given `FRAGMENT-EDGES' an ALIST description of a weighted graph, fill out the `ADJACENT-EDGES' hash tables for each of the `NODES'."
  (loop :for node :in nodes
        :for adjacent-edges := (fragment-node-adjacent-edges node)
        :for edges-for-node := (cadr (assoc (fragment-node-id node) fragment-edges))
        :do (loop :for (id weight) :in edges-for-node
                  :for edge-data := (make-fragment-edge-data :edge-weight weight)
                  :for edge := (process-public-address (nth id nodes))
                  :do (setf (gethash edge adjacent-edges) edge-data))))

(defun simulation-run-until-core-halts (simulation nodes core-id-0 core-id-1)
  "Run `SIMULATION' until the two `NODES' specified by indices `CORE-ID-0' and `CORE-ID-1' die."
  (simulation-run simulation :canary (canary-all
                                      (canary-process (nth core-id-0 nodes))
                                      (canary-process (nth core-id-1 nodes)))))

(defun compute-spanning-tree-weight (root &optional from-node)
  "Compute the weight of the spanning tree rooted at `ROOT'. Done recursively, and `FROM-NODE' indicates where we came from (and thus we should skip this edge when recursing. WARNING: Only works when used inside `WITH-ADDRESS-DEREFERENCING'."
  (with-slots (adjacent-edges) root
    (let ((branches (find-specific-edges adjacent-edges
                                         :desired-state ':BRANCH
                                         :not-edge from-node)))
      (cond
        ((endp branches)
         (return-from compute-spanning-tree-weight 0))
        (t
         (let ((weight-of-branches (loop :for branch :in branches
                                         :sum (fragment-edge-data-edge-weight
                                               (gethash branch adjacent-edges))))
               (weight-of-rest (loop :for branch :in branches
                                     :sum (compute-spanning-tree-weight
                                           (dereference branch)
                                           (process-public-address root)))))
           (return-from compute-spanning-tree-weight (+ weight-of-branches
                                                        weight-of-rest))))))))

;;;
;;; test definitions
;;;

(deftest test-distributed-mst-2-line ()
  "Test that we can run Gallager's distributed MST algorithm until the core `FRAGMENT-NODE's halt, and then the `:BRANCH' edges encode the MST. Uses a problem graph of 2 nodes in a line:

0-[1]-1"
  (with-transient-logger ()
    (with-address-dereferencing ()
      (with-courier ()
        (with-simulation (simulation (*local-courier*))
          (let* ((fragment-edges '((0 ((1 1)))
                                   (1 ((0 1)))))
                 (nodes (add-fragment-nodes simulation fragment-edges)))
            (simulation-run-until-core-halts simulation nodes 0 1)
            (print-message-report)
            (is (= 1 (compute-spanning-tree-weight (first nodes))))))))))

(deftest test-distributed-mst-3-line ()
  "Test that we can run Gallager's distributed MST algorithm until the core `FRAGMENT-NODE's halt, and then the `:BRANCH' edges encode the MST. Uses a problem graph of 3 nodes in a line:

0-[1]-1-[2]-2"
  (with-transient-logger ()
    (with-address-dereferencing ()
      (with-courier ()
        (with-simulation (simulation (*local-courier*))
          (let* ((fragment-edges '((0 ((1 1)))
                                   (1 ((0 1) (2 2)))
                                   (2 ((1 2)))))
                 (nodes (add-fragment-nodes simulation fragment-edges)))
            (simulation-run-until-core-halts simulation nodes 0 1)
            (print-message-report)
            (is (= 3 (compute-spanning-tree-weight (first nodes))))))))))

(deftest test-distributed-mst-4-line ()
  "Test that we can run Gallager's distributed MST algorithm until the core `FRAGMENT-NODE's halt, and then the `:BRANCH' edges encode the MST. Uses a problem graph of 4 nodes in a line:

0-[1]-1-[3]-2-[2]-3

This will be the first problem graph that needs to change core: fragments (0-1) and (2-3) will merge to form a level-2 fragment with core (1-2)."
  (with-transient-logger ()
    (with-address-dereferencing ()
      (with-courier ()
        (with-simulation (simulation (*local-courier*))
          (let* ((fragment-edges '((0 ((1 1)))
                                   (1 ((0 1) (2 3)))
                                   (2 ((1 3) (3 2)))
                                   (3 ((2 2)))))
                 (nodes (add-fragment-nodes simulation fragment-edges)))
            (simulation-run-until-core-halts simulation nodes 1 2)
            (print-message-report)
            (is (= 6 (compute-spanning-tree-weight (first nodes))))))))))

(deftest test-distributed-mst-3-triangle ()
  "Test that we can run Gallager's distributed MST algorithm until the core `FRAGMENT-NODE's halt, and then the `:BRANCH' edges encode the MST. Uses a problem graph of 3 nodes that form a triangle:

0-[1]-1-[2]-2-[3]-0

This will be the first problem graph that marks an edge as Rejected."
  (with-transient-logger ()
    (with-address-dereferencing ()
      (with-courier ()
        (with-simulation (simulation (*local-courier*))
          (let* ((fragment-edges '((0 ((1 1) (2 3)))
                                   (1 ((0 1) (2 2)))
                                   (2 ((1 2) (0 3)))))
                 (nodes (add-fragment-nodes simulation fragment-edges)))
            (simulation-run-until-core-halts simulation nodes 0 1)
            (print-message-report)
            (is (= 3 (compute-spanning-tree-weight (first nodes))))))))))

(deftest test-distributed-mst-4-complete ()
  "Test that we can run Gallager's distributed MST algorithm until the core `FRAGMENT-NODE's halt, and then the `:BRANCH' edges encode the MST. Uses a problem graph of 4 nodes that are fully connected:

 0 -[1]- 1
 |       |
[5]     [6]
 |       |
 3 -[2]- 2

 0 -[4]- 2
 1 -[3]- 3"
  (with-transient-logger ()
    (with-address-dereferencing ()
      (with-courier ()
        (with-simulation (simulation (*local-courier*))
          (let* ((fragment-edges '((0 ((1 1) (2 4) (3 5)))
                                   (1 ((0 1) (2 6) (3 3)))
                                   (2 ((1 6) (3 2) (0 4)))
                                   (3 ((2 2) (0 5) (1 3)))))
                 (nodes (add-fragment-nodes simulation fragment-edges)))
            (simulation-run-until-core-halts simulation nodes 1 3)
            (print-message-report)
            (is (= 6 (compute-spanning-tree-weight (first nodes))))))))))

(deftest test-distributed-mst-10-mcgill ()
  "Test that we can run Gallager's distributed MST algorithm until the core `FRAGMENT-NODE's halt, and then the `:BRANCH' edges encode the MST. Uses a problem graph of 10 nodes:

a b w
0 5 2
5 6 7
6 7 15
7 9 13
9 8 9
8 2 18
2 1 17
1 0 3
4 5 1
4 6 6
4 7 5
4 8 10
4 3 11
8 7 12
3 8 4
3 2 8
3 1 16

graph-1 from: https://github.com/francesco-bongiovanni/Distributed-Graph-Algorithms/blob/master/Minimum-Spanning-Tree/ReadMe.md"
  (with-transient-logger ()
    (with-address-dereferencing ()
      (with-courier ()
        (with-simulation (simulation (*local-courier*))
          (let* ((fragment-edges '((0 ((5  2) (1  3)))
                                   (1 ((2 17) (0  3) (3 16)))
                                   (2 ((8 18) (1 17) (3  8)))
                                   (3 ((4 11) (8  4) (2  8) (1 16)))
                                   (4 ((5  1) (6  6) (7  5) (8 10) (3 11)))
                                   (5 ((0  2) (6  7) (4  1)))
                                   (6 ((5  7) (7 15) (4  6)))
                                   (7 ((6 15) (9 13) (4  5) (8 12)))
                                   (8 ((9  9) (2 18) (4 10) (7 12) (3 4)))
                                   (9 ((7 13) (8  9)))))
                 (nodes (add-fragment-nodes simulation fragment-edges)))
            (simulation-run-until-core-halts simulation nodes 4 8)
            (print-message-report)
            (is (= 48 (compute-spanning-tree-weight (first nodes))))))))))
