;;;; network/ordinary.lisp
;;;;
;;;; A stock example for a diagonal-nearest-neighbor networked family of couriers in a square grid.
;;;;
;;;; Compared to courier-gridded, this raises the vertex degree of a courier from 4 to 8, but for
;;;; two nodes (x0, y0) and (x1, y1), it reduces the hop distance from |x0 - x1| + |y0 - y1| to
;;;; max(|x0 - x1|, |y0 - y1|).

(in-package #:aether)

(defstruct ordinal-neighbors
  "A structure for storing the neighbors of a courier participating in a grid."
  left
  right
  up
  down
  left-up
  left-down
  right-up
  right-down)

(defstruct (courier-ordinal (:include courier) (:constructor %make-courier-ordinal))
  "A `COURIER' instance networked to other couriers in a grid.

NOTE: Expects `ID' to be a list and `NEIGHBORS' to be a `ORDINAL-NEIGHBORS'.")

(defun make-courier-ordinal (&rest initargs)
  (initialize-and-return ((courier (apply #'%make-courier-ordinal initargs)))
    (unless (getf initargs ':neighbors)
      (setf (courier-neighbors courier) (make-ordinal-neighbors)))))

(defmethod courier-courier->route ((processing-courier courier-ordinal) destination-courier-id)
  (with-slots (left right up down left-down right-down left-up right-up)
      (courier-neighbors processing-courier)
    (destructuring-bind (dx dy) destination-courier-id
      (destructuring-bind (px py) (courier-id processing-courier)
        (cond
          ((and (< dx px) (< dy py)) left-down)
          ((and (> dx px) (< dy py)) right-down)
          ((and (< dx px) (> dy py)) left-up)
          ((and (> dx px) (> dy py)) right-up)
          ((and (< dx px) (= dy py)) left)
          ((and (> dx px) (= dy py)) right)
          ((and (= dx px) (< dy py)) down)
          ((and (= dx px) (> dy py)) up)
          (t
           (warn "Requested to route a message that's already at its destination.")
           processing-courier))))))

(defun make-courier-ordinal-grid (size-i size-j)
  "Constructs a (size-i x size-j) grid of COURIER-GRIDDED instances."
  (initialize-and-return ((courier-list)
                          (grid (make-array (list size-i size-j))))
    (dotimes (i size-i)
      (dotimes (j size-j)
        (let ((courier (make-courier-ordinal :id (list i j))))
          (setf (aref grid i j) courier)
          (push courier courier-list))))
    (dotimes (i size-i)
      (dotimes (j size-j)
        (let ((left       (and (<= 0 (1- i))                       (aref grid (1- i) j)))
              (right      (and (< (1+ i) size-i)                   (aref grid (1+ i) j)))
              (down       (and                   (<= 0 (1- j))     (aref grid i      (1- j))))
              (up         (and                   (< (1+ j) size-j) (aref grid i      (1+ j))))
              (left-down  (and (<= 0 (1- i))     (<= 0 (1- j))     (aref grid (1- i) (1- j))))
              (left-up    (and (<= 0 (1- i))     (< (1+ j) size-j) (aref grid (1- i) (1+ j))))
              (right-down (and (< (1+ i) size-i) (<= 0 (1- j))     (aref grid (1+ i) (1- j))))
              (right-up   (and (< (1+ i) size-i) (< (1+ j) size-j) (aref grid (1+ i) (1+ j)))))
          (let ((neighbors (courier-neighbors (aref grid i j))))
            (setf (ordinal-neighbors-left  neighbors)      left
                  (ordinal-neighbors-right neighbors)      right
                  (ordinal-neighbors-up    neighbors)      up
                  (ordinal-neighbors-down  neighbors)      down
                  (ordinal-neighbors-left-up neighbors)    left-up
                  (ordinal-neighbors-left-down neighbors)  left-down
                  (ordinal-neighbors-right-up neighbors)   right-up
                  (ordinal-neighbors-right-down neighbors) right-down)))))))
