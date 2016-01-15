
;;;; 
;;;; Board stuff
;;;; 

(in-package :board)

(defconstant BLACK 'B)
(defconstant WHITE 'W)
(defconstant EMPTY '_)
(defconstant BORDER 'X)
(if (not (symbolp :olli))
(defconstant OLLI '"Olli")
)

(defun get-width (board)
  "Get width of a board (1..x)"
  (+ (array-dimension board 1) -2))

(defun get-height (board)
  "Get height of a board (1..x)"
  (+ (array-dimension board 0) -2))

(defun get-field (board x y)
  "Get the value of a field"
  (aref board (+ 1 y) (+ 1 x)))

(defun create-board (width height)
  "Create a board"
  (let ( (board (make-array (list (+ 2 height) (+ 2 width)) :initial-element EMPTY)))
    (dotimes (x (+ 2 width))
      (setf (aref board 0 x) BORDER)
      (setf (aref board (+ height 1) x) BORDER)
      )
    (dotimes (y (+ 2 height))
      (setf (aref board y 0) BORDER)
      (setf (aref board y (+ width 1)) BORDER)
      )
    board))

(defun clone-board (board)
  "Clone a board. todo: better implementation"
  (let ((new-board (create-board (get-width board) (get-height board))))
    (dotimes (x (+ 2 (get-width board)))
      (dotimes (y (+ 1 (get-height board)))
	(setf (aref new-board y x) (aref board y x))
	))
    new-board))

(defun set-field (board x y color)
  "Set a field of the board. Non-Destructive"
  (let ((new-board (clone-board board)))
    (setf (aref new-board (+ 1 y) (+ 1 x)) color)
    new-board))

(defun clear-field (board x y)
  "Clear a field of the board. Non-Destructive"
  (set-field board x y EMPTY))

(defun nset-field (board x y color)
  "Set a field of the board. Destructive"
  (setf (aref board (+ 1 y) (+ 1 x)) color)
  board)

(defun nclear-field (board x y)
  "Clear a field of the board. Destructive"
  (nset-field board x y EMPTY))

(defun scan (board x y dx dy color)
  "Determine the connected pieces for the given position, direction and color"
  ;; x y: Starting point from which adjacent points are checked for the same color
  ;; dx dy: Direction to traverse ("as is" and inverted)
  ;; returns list of (x y) tupels
  (let ((length '())) 
    (labels (
	     (traverse (x y dx dy)
		       (declare (fixnum x y dx dy))
		       (if (eq (get-field board x y) color)
			   (progn
			     (push (list x y) length)
			     (traverse (+ x dx) (+ y dy) dx dy)
			     ))))
	    ;; traverse initial direction
	    (traverse x y dx dy)
	    ;; traverse inverted direction
	    (setf dx (* dx -1))
	    (setf dy (* dy -1))
	    ;; step away from initial position that has already been checked and traverse once more
	    (traverse (+ x dx) (+ y dy) dx dy)
	    )
    length))

(defun get-connected-pieces (board x y)
  "Returns a list of tupels (x y) which describe the most length sequence 
of connected pieces for the given position."
  (if (not (field-set-p board x y))
      nil
      (let ( (all '()))
	(dolist (d '((0 1) (1 0) (1 1) (1 -1)))
	  (push (scan board x y (first d) (second d) (get-field board x y)) all))
	(reduce (lambda (best item)
		  (if (> (length best) (length item)) best item)) 
		all))))

(defun field-set-p (board x y)
  "Check if given field is BLACK or WHITE."
  (if (not (eq (get-field board x y) EMPTY)) t NIL))

(defun drop (board x)
  "Calculate the row into which a piece will fall if its thrown into the given column. 
Returns nil if no place left in column."
  (if (not (field-set-p board x 0))
      (+ (length (scan board x 0 0 1 EMPTY)) -1)
    nil))

