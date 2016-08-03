
(in-package :engine)

(defclass board-controller ()
  (
   (board :initform nil)
   (validate :initarg :validate :initform t)
   (count :initform 0)
   (stack :initform '())
   ))

(defmethod initialize-instance :after ((controller board-controller) &key board)
  (setf (slot-value controller 'board) (clone-board board)))

(defgeneric get-board (board-controller)
  (:documentation ""
  ))

(defgeneric set-boardfield (board-controller x y token)
  (:documentation ""
  ))

(defgeneric undo-set-boardfield (board-controller)
  (:documentation ""
  ))

(defgeneric get-stack (board-controller)
  (:documentation ""
  ))

(defgeneric get-count (board-controller))

(defmethod get-board ((controller board-controller))
  (slot-value controller 'board))

(defmethod get-count ((controller board-controller))
  (slot-value controller 'count))

(defmethod get-stack ((controller board-controller))
  (slot-value controller 'stack))

(defmethod set-boardfield ((controller board-controller) x y token)
  (if (slot-value controller 'validate)
      (if (field-set-p (slot-value controller 'board) x y)
	  (error "Field is not empty")))
  (nset-field (slot-value controller 'board) x y token)
  (setf (slot-value controller 'count) (+ 1 (slot-value controller 'count)))
  (push (list x y token) (slot-value controller 'stack))
  (slot-value controller 'board))

(defmethod undo-set-boardfield ((controller board-controller))
  (if (slot-value controller 'validate)
      (if (eql nil (slot-value controller 'stack))
	  (error "No more move to undo")))
  (let ((field (pop (slot-value controller 'stack))))
    (nclear-field (slot-value controller 'board) (first field) (second field)))
  (slot-value controller 'board))

