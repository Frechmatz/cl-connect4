#|
Extremely inefficient bozo implementation of a queue
|#

(in-package :ccfi)

(defclass queue ()
  ((q :initform '())))

(defgeneric put (queue item))
(defgeneric next (queue))
(defgeneric peek (queue))

(defmethod put ((the-queue queue) item)
  (setf (slot-value the-queue 'q) (append (slot-value the-queue 'q) (list item) )))

(defmethod next ((the-queue queue))
  (let ((i (car (slot-value the-queue 'q))))
    (setf (slot-value the-queue 'q) (cdr (slot-value the-queue 'q)))
    i))

(defmethod peek ((the-queue queue))
  (car (slot-value the-queue 'q)))
