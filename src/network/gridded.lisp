;;;; network/gridded.lisp
;;;;
;;;; A stock example for a nearest-neighbor networked family of couriers in a square grid.

(in-package #:aether)

(defstruct grid-neighbors
  "A structure for storing the neighbors of a courier participating in a grid."
  left
  right
  up
  down)

(defstruct (courier-gridded (:include courier) (:constructor %make-courier-gridded))
  "A `COURIER' instance networked to other couriers in a grid.

NOTE: Expects `ID' to be a list and `NEIGHBORS' to be a `GRID-NEIGHBORS'.")

(defun make-courier-gridded (&rest initargs)
  (initialize-and-return ((courier (apply #'%make-courier-gridded initargs)))
    (unless (getf initargs ':neighbors)
      (setf (courier-neighbors courier) (make-grid-neighbors)))))

;; NOTE: For funsies, I tried making routing nondeterministic, by gathering all
;;     the possible options and picking randomly among them.  This increased
;;     network pressure in the test by 25%.  Still, I can't imagine that the
;;     routing rules below are themselves wise.
(defmethod courier-courier->route ((processing-courier courier-gridded) destination-courier-id)
  (with-slots (left right up down) (courier-neighbors processing-courier)
    (destructuring-bind (dx dy) destination-courier-id
      (destructuring-bind (px py) (courier-id processing-courier)
        (cond
          ((< dx px) left)
          ((> dx px) right)
          ((< dy py) down)
          ((> dy py) up)
          (t
           (warn "Requested to route a message that's already at its destination.")
           processing-courier))))))

(defun make-courier-grid (size-i size-j)
  "Constructs a (size-i x size-j) grid of COURIER-GRIDDED instances."
  (initialize-and-return ((grid (make-array (list size-i size-j))))
    (dotimes (i size-i)
      (dotimes (j size-j)
        (setf (aref grid i j) (make-courier-gridded :id (list i j)))))
    (dotimes (i size-i)
      (dotimes (j size-j)
        (let ((left  (and (<= 0 (1- i))     (aref grid (1- i) j)))
              (right (and (< (1+ i) size-i) (aref grid (1+ i) j)))
              (down  (and (<= 0 (1- j))     (aref grid i      (1- j))))
              (up    (and (< (1+ j) size-j) (aref grid i      (1+ j)))))
          (let ((neighbors (courier-neighbors (aref grid i j))))
            (setf (grid-neighbors-left  neighbors) left
                  (grid-neighbors-right neighbors) right
                  (grid-neighbors-up    neighbors) up
                  (grid-neighbors-down  neighbors) down)))))))
