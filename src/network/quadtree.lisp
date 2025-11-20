;;;; network/quadtree.lisp
;;;;
;;;; A stock example for a quadtree networked family of couriers in a square grid.
;;;;
;;;; A typical family of quadtree routers looks like:
;;;;
;;;;         +-+-+-+-+         +---+---+        +-------+
;;;;        /A/B/C/D/         /   /   /        /       /
;;;;       +-+-+-+-+         / 1 / 2 /        /       /
;;;;      /E/F/G/H/         /   /   /        /       /
;;;;     +-+-+-+-+         +---+---+        /  TOP  /
;;;;    /I/J/K/L/         /   /   /        /       /
;;;;   +-+-+-+-+         / 3 / 4 /        /       /
;;;;  /M/N/O/P/         /   /   /        /       /
;;;; +-+-+-+-+         +---+---+        +-------+
;;;;
;;;; where each cell contains a router which is connected to the cell immediately above it in the
;;;; hierarchy (i.e., to the right) with which contains it and to all the cells immediately below it
;;;; in the hierarchy (i.e., to the left) which are contained by it.  The finest-grained domains
;;;; (A, ..., P) with no downward links are referred to as 'leaves'.
;;;;
;;;; Message routing follows containments rather than spatial structure.  For instance, a message
;;;; sent from A to P takes the following steps:
;;;; * Inspect A's routing table, which only has an upward connection.  Follow it and deliver the
;;;;   message to 1.
;;;; * Inspect 1's routing table, which has links to A, B, E, F, and upward.  P is not contained in
;;;;   any of the downward directions, so default to the upward direction.  Follow it and deliver
;;;;   the message to TOP.
;;;; * Inspect TOP's routing table, which has links to 1, 2, 3, and 4.  P is contained in 4, so
;;;;   deliver the message to 4.
;;;; * Inspect 4's routing table, which has has links to K, L, O, P, and an upward link back to TOP.
;;;;   P is contained in P, so deliver the message to P.

(in-package #:aether)

(defstruct (rectangle)
  "Tracks the domain of a quadtree subrouter."
  (left 0 :type integer)
  (right 0 :type integer)
  (top 0 :type integer)
  (bottom 0 :type integer))

(defun rectangles-meet? (rect1 rect2)
  "Tests whether `RECT1' and `RECT2' intersect."
  (when (or (eq ':otherwise rect1) (eq ':otherwise rect2))
    (return-from rectangles-meet? t))
  (not (or (< (rectangle-right rect2) (rectangle-left rect1))
           (< (rectangle-right rect1) (rectangle-left rect2))
           (< (rectangle-top rect2) (rectangle-bottom rect1))
           (< (rectangle-top rect1) (rectangle-bottom rect2)))))

(defun rectangle-member? (rect point)
  "Tests `POINT''s membership in `RECT'."
  (when (eq ':otherwise rect)
    (return-from rectangle-member? t))
  (destructuring-bind (x y) point
    (and (<= (rectangle-left   rect) x (rectangle-right rect))
         (<= (rectangle-bottom rect) y (rectangle-top   rect)))))

(defstruct (courier-quadtree (:include courier) (:constructor %make-courier-quadtree))
  "A `COURIER' instance networked to others along a quadtree."
  (links nil :type list))

(defun make-courier-quadtree (size-x size-y)
  "Makes a whole quadtree apparatus stretching from (0, 0) to (size-x - 1, size-y - 1).

Returns a VALUES pair: the 2d array of leaf routers (to use as the local courier for worker processes), then the list of *all* routers (to use to prime the simulator)."
  (let ((rectangle (make-rectangle :left 0 :bottom 0
                                   :right (1- size-x) :top (1- size-y))))
    (make-courier-quadtree-rectangle rectangle nil)))

(defun make-courier-quadtree-rectangle (rectangle parent)
  "Makes one quadtree router and its children.

Returns a VALUES pair: the 2d array of leaf routers (to use as the local courier for worker processes), then the list of *all* routers (to use to prime the simulator)."
  ;; this actual routine is only responsible for generating the 'root' router.
  ;; all the other routers in the tree are deferred to recursive calls.
  (with-slots ((max-x right) (max-y top) (min-x left) (min-y bottom)) rectangle
    (assert (<= min-x max-x))
    (assert (<= min-y max-y))
    ;; the strategy is to divide RECTANGLE up into quarters, stored in SUBRECTANGLES.
    ;; the relevant edge case is when RECTANGLE has width or height 1, in which case subdivision
    ;; doesn't make sense.
    (initialize-and-return
        ((courier (%make-courier-quadtree :links `((:otherwise ,parent))))
         (half-width  (floor (/ (- max-x min-x) 2)))
         (half-height (floor (/ (- max-y min-y) 2)))
         subrectangles
         (flat-courier-list '())
         ;; NOTE: this is so that :otherwise has lowest precedence order.
         ;;       could also handle this as a slot on courier-quadtree, which might be nicer.
         (leaf-courier-array (make-array `(,(1+ (- max-x min-x)) ,(1+ (- max-y min-y))))))
      (cond
        ((and (= max-x min-x) (= max-y min-y))
         nil)
        ((= max-x min-x)
         (setf subrectangles
               (list (make-rectangle :right min-x
                                     :top (+ min-y half-height)
                                     :left min-x
                                     :bottom min-y)
                     (make-rectangle :right min-x
                                     :top max-y
                                     :left min-x
                                     :bottom (+ 1 min-y half-height)))))
        ((= max-y min-y)
         (setf subrectangles
               (list (make-rectangle :right (+ min-x half-width)
                                     :top min-y
                                     :left min-x
                                     :bottom min-y)
                     (make-rectangle :right max-x
                                     :top min-y
                                     :left (+ 1 min-x half-width)
                                     :bottom min-y))))
        (t
         (setf subrectangles
               (list (make-rectangle :right (+ min-x half-width)
                                     :top (+ min-y half-height)
                                     :left min-x
                                     :bottom min-y)
                     (make-rectangle :right (+ min-x half-width)
                                     :top max-y
                                     :left min-x
                                     :bottom (+ 1 min-y half-height))
                     (make-rectangle :right max-x
                                     :top (+ min-y half-height)
                                     :left (+ 1 min-x half-width)
                                     :bottom min-y)
                     (make-rectangle :right max-x
                                     :top max-y
                                     :left (+ 1 min-x half-width)
                                     :bottom (+ 1 min-y half-height))))))
      
      ;; having partitioned RECTANGLE into SUBRECTANGLES, we:
      ;; * recursively build sub-quadtrees for each subrectangle.
      ;; * stash the root of each sub-quadtree for the routing table at the router we're building.
      (dolist (sr subrectangles)
        (with-slots ((submax-x right) (submax-y top) (submin-x left) (submin-y bottom)) sr
          ;; each of these calls generates the advertised VALUES pair:
          ;; a 2D array of leaf routers and a flat list of all routers in the subtree.
          (multiple-value-bind (subcourier-array subcourier-list)
              (make-courier-quadtree-rectangle
               (make-rectangle :left   submin-x :right submax-x
                               :bottom submin-y :top   submax-y)
               courier)
            ;; stash the routing table entry
            (push `(,sr ,(first subcourier-list)) (courier-quadtree-links courier))
            ;; accrue onto the flat list of all routers
            (setf flat-courier-list (nconc subcourier-list flat-courier-list))
            ;; copy the 2D array of sub-leaf-routers into the relevant subarray of leaf routers
            (dotimes (i (1+ (- submax-x submin-x)))
              (dotimes (j (1+ (- submax-y submin-y)))
                (setf (aref leaf-courier-array (+ submin-x i (- min-x)) (+ submin-y j (- min-y)))
                      (aref subcourier-array i j)))))))
      
      ;; base case: we add the router we're constructing to the two data structures to return.
      ;; we always belong in the flat list of all routers.
      (push courier flat-courier-list)
      ;; and, if we're a leaf (i.e., we have no subrectangles), we belong in the 2D array.
      (unless subrectangles
        (setf (aref leaf-courier-array 0 0) courier
              (courier-id courier) `(,min-x ,min-y))))))

(defmethod courier-courier->route ((processing-courier courier-quadtree) destination-courier-id)
  "Routes according to a rectangle-membership-based routing table."
  
  (loop :for (rect link) :in (courier-quadtree-links processing-courier)
        :when (rectangle-member? rect destination-courier-id)
          :do (return-from courier-courier->route link))
  (break)
  (warn "Requested to route a message that's already at its destination: ~a vs ~a."
        destination-courier-id (courier-quadtree-links processing-courier))
  processing-courier)
