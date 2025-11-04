;;;; network/quadtree.lisp
;;;;
;;;; A stock example for a quadtree networked family of couriers in a square grid.

(in-package #:aether)

(defstruct (rectangle)
  ""
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
  (let ((rectangle (make-rectangle :left 0 :bottom 0
                                   :right (1- size-x) :top (1- size-y))))
    (make-courier-quadtree-rectangle rectangle nil)))

(defun make-courier-quadtree-rectangle (rectangle parent)
  ""
  (with-slots ((max-x right) (max-y top) (min-x left) (min-y bottom)) rectangle
    (assert (<= min-x max-x))
    (assert (<= min-y max-y))
    (let ((courier (%make-courier-quadtree :links `((:otherwise ,parent))))
          (half-width  (floor (/ (- max-x min-x) 2)))
          (half-height (floor (/ (- max-y min-y) 2)))
          subrectangles
          ;; NOTE: this is so that :otherwise has lowest precedence order.
          ;;       could also handle this as a slot on courier-quadtree, which might be nicer.
          (leaf-courier-array (make-array `(,(1+ (- max-x min-x)) ,(1+ (- max-y min-y)))))
          (flat-courier-list '()))
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
      
      (dolist (sr subrectangles)
        (with-slots ((submax-x right) (submax-y top) (submin-x left) (submin-y bottom)) sr
          (multiple-value-bind (subcourier-array subcourier-list)
              (make-courier-quadtree-rectangle
               (make-rectangle :left   submin-x :right submax-x
                               :bottom submin-y :top   submax-y)
               courier)
            (push `(,sr ,(first subcourier-list)) (courier-quadtree-links courier))
            (setf flat-courier-list (nconc subcourier-list flat-courier-list))
            (dotimes (i (1+ (- submax-x submin-x)))
              (dotimes (j (1+ (- submax-y submin-y)))
                (setf (aref leaf-courier-array (+ submin-x i (- min-x)) (+ submin-y j (- min-y)))
                      (aref subcourier-array i j)))))))
      
      (push courier flat-courier-list)
      ;; base case: poke ourselves into the array as a leaf
      (unless subrectangles
        (setf (aref leaf-courier-array 0 0) courier
              (courier-id courier) `(,min-x ,min-y)))
      (values leaf-courier-array flat-courier-list))))

(defmethod courier-courier->route ((processing-courier courier-quadtree) destination-courier-id)
  "Routes according to a rectangle-membership-based routing table."
  (loop :for (rect link) :in (courier-quadtree-links processing-courier)
        :when (rectangle-member? rect destination-courier-id)
          :do (return-from courier-courier->route link))
  (break)
  (warn "Requested to route a message that's already at its destination: ~a vs ~a."
        destination-courier-id (courier-quadtree-links processing-courier))
  processing-courier)
