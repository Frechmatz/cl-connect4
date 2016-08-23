
(in-package :engine)

(defclass board-controller ()
  ((board :initform nil)
   (validate :initarg :validate :initform t)
   (count :initform 0)
   (path :initform '()))
  (:documentation
   "Class to play moves on a board. Moves can be undone. The current move sequence (the path)
    is kept and can be decorated, for example with a board score. Also a total count of 
    moves (plies) is provided.")
  )

(defmethod initialize-instance :after ((controller board-controller) &key board)
  (setf (slot-value controller 'board) (clone-board board)))

(defgeneric get-board (board-controller)
  (:documentation "Get the current board"
  ))

(defgeneric set-boardfield (board-controller x y token)
  (:documentation "Set a field"
  ))

(defgeneric undo-set-boardfield (board-controller)
  (:documentation "Undo the last move."
  ))

(defgeneric get-path (board-controller)
  (:documentation "Get the current move sequence, aka the path. The path
is represented by a list of (x y color <decoration>). The first entry of the list
represents the first move made."
  ))

(defgeneric decorate-path (board-controller value)
  (:documentation "Decorate the latest move with a value. Will be provided as fourth
entry for each path segment."
  ))

(defgeneric get-count (board-controller)
  (:documentation
   "Get the total count of moves (plies) that have been made. Undoing a move
    doesn't effect this count"))

(defmethod get-board ((controller board-controller))
  (slot-value controller 'board))

(defmethod get-count ((controller board-controller))
  (slot-value controller 'count))

(defmethod get-path ((controller board-controller))
  ;; return a copy because the path slot
  ;; will be modified when fields are set and unset.
  (copy-list (slot-value controller 'path)))

(defmethod set-boardfield ((controller board-controller) x y token)
  (if (slot-value controller 'validate)
      (if (field-set-p (slot-value controller 'board) x y)
	  (error "Field is not empty")))
  (nset-field (slot-value controller 'board) x y token)
  (setf (slot-value controller 'count) (+ 1 (slot-value controller 'count)))
  (push (list x y token) (slot-value controller 'path))
  (slot-value controller 'board))

(defmethod undo-set-boardfield ((controller board-controller))
  (if (slot-value controller 'validate)
      (if (eql nil (slot-value controller 'path))
	  (error "No more move to undo")))
  (let ((field (pop (slot-value controller 'path))))
    (nclear-field (slot-value controller 'board) (first field) (second field)))
  (slot-value controller 'board))

(defmethod decorate-path ((controller board-controller) value)
  (let ((path (slot-value controller 'path)))
    (if path
	(let ((item (first path)))
	  (setf (slot-value controller 'path)
		(concatenate 'list (list (list (first item) (second item) (third item) value)) (cdr path)))))))
