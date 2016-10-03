;;;; 
;;;; Board implementation
;;;; 

(in-package :board)

(defconstant BLACK 'B)
(defconstant WHITE 'W)
(defconstant EMPTY '_)
(defconstant BORDER 'X)

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

(defun scan-board (board x y dx dy continueFn)
  "Scan the board.
   x y: Start position
   dx dy: Direction to traverse (bi-directional)
   continueFn: Callback function that is called with the color
     of the current field. If the function returns a non-nil
     value the scanning of the current direction is continued.
  returns a list of (x y) tuples of matching fields"
  (let ((fields '())) 
    (labels (
	     (traverse (x y dx dy)
		       (declare (fixnum x y dx dy))
		       (if (funcall continueFn (get-field board x y))
			   (progn
			     (push (list x y) fields)
			     (traverse (+ x dx) (+ y dy) dx dy)))))
	    ;; traverse initial direction
	    (traverse x y dx dy)
	    ;; traverse inverted direction
	    (setf dx (* dx -1))
	    (setf dy (* dy -1))
	    ;; step away from initial position that has already been checked and traverse once more
	    (traverse (+ x dx) (+ y dy) dx dy))
    fields))

(defun scan-board-all-directions (board x y continueFn)
  "Scan the board in all directions
   x y continueFn: See function scan-board
   returns a list of ((dx dy) ((<matching field>)))"
  (let ( (all '()))
    (dolist (d '((0 1) (1 0) (1 1) (1 -1)))
      (push
       (list
	d
	(scan-board
	     board x y
	     (first d)
	     (second d)
	     continueFn))
       all))
    all))

(defun get-connected-pieces (board x y)
  "Returns a list of tupels (x y) which represents the most length sequence 
   of connected pieces for the given position."
  (if (not (field-set-p board x y))
      nil
      (let ((all (scan-board-all-directions
		  board
		  x
		  y
		  (lambda (color) (eq color (get-field board x y))))))
	(second ;; skip the direction vector
	 (reduce (lambda (best item)
		   (if (> (length (second best)) (length (second item))) best item)) 
		all)))))

(defun field-set-p (board x y)
  "Check if given field is BLACK or WHITE."
  (if (not (eq (get-field board x y) EMPTY)) t NIL))

(defun drop (board x)
  "Calculate the row into which a piece will fall if its thrown into the given column. 
Returns nil if no place left in column."
  (if (not (field-set-p board x 0))
      (+ (length (scan-board
		  board
		  x 0 0 1
		  (lambda (color) (eq color EMPTY)))) -1)
    nil))

