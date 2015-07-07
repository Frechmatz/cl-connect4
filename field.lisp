

#|
Common Connect4 board related functionality
- Encapsulates the representation of the board
- Provides functions to create, clone, access and manipulate a board
- Provides a formatter class for pretty printing a board
|#

(defparameter *BLACK* 'B)
(defparameter *WHITE* 'W)
(defparameter *EMPTY* '_)
(defparameter *BORDER* 'X)

;;
;; Get width of a board (1..x)
;;
(defun get-board-width (board)
  (+ (array-dimension board 1) -2))

(defun get-max-x (board)
  (+  (get-board-width board) -1))

;;
;; Get height of a board (1..x)
;; 
(defun get-board-height (board)
  (+ (array-dimension board 0) -2))

(defun get-max-y (board)
  (+  (get-board-height board) -1))

(defun get-field (board x y)
  (aref board (+ 1 y) (+ 1 x)))


;;
;; Create a board
;; 
(defun create-board (width height)
  ;;  (let ( (board (make-array `( ,(+ 2 *HEIGHT*) ,(+ 2 *WIDTH*)) :initial-element *EMPTY*)))
  (let ( (board (make-array (list (+ 2 height) (+ 2 width)) :initial-element *EMPTY*)))
    (dotimes (x (+ 2 width))
      (setf (aref board 0 x) *BORDER*)
      (setf (aref board (+ height 1) x) *BORDER*)
      )
    (dotimes (y (+ 2 height))
      (setf (aref board y 0) *BORDER*)
      (setf (aref board y (+ width 1)) *BORDER*)
      )
    board
    ))

;;
;; Clone a board
;; 
(defun clone-board (board)
  ;; Bozo algorithm :(
  (let ((new-board (create-board (get-board-width board) (get-board-height board))))
    (dotimes (x (+ 2 (get-board-width board)))
      (dotimes (y (+ 1 (get-board-height board)))
	(setf (aref new-board y x) (aref board y x))
	))
    new-board))

;;
;; Clone board and set field
;; returns new board
;; 
(defun set-field (board x y color)
  (let ((new-board (clone-board board)))
    (setf (aref new-board (+ 1 y) (+ 1 x)) color)
    new-board
    ))

;;
;; Set field in given board (does not clone the board)
;; returns manipulated board
;;
(defun nset-field (board x y color)
  (setf (aref board (+ 1 y) (+ 1 x)) color)
  board
    )

;;
;; Check if a field has a given color
;; applies border checking and returns nil if field is out of board
;; this check is a bit costly but allows simpler implementations of
;; board traversals
;;
(defun is-field-color-p (board x y color)
  (if (or (>= x (get-board-width board)) (>= y (get-board-height board)))
      nil
    (eq (get-field board x y) color)
  ))


;;;
;;; Calculate the total length of the line at the given position and for given direction
;;; x y: Starting point from which adjacent points are checked for the same color
;;; dx dy: Direction to traverse ("as is" and inverted)
;;;
(defun line-length-at (board x y dx dy color)
  (declare (fixnum x y dx dy))
  (let ((length 0)) 
    (labels (
	   (traverse (x y dx dy)
	       (declare (fixnum x y dx dy))
	       (if (is-field-color-p board x y color)
		   (progn
		     (setf length (+ length 1))
		     ;; boundary checking is applied by is-field-color-p
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
    	  length
	  ))


(defun is-field-set (board x y)
  (if (or (is-field-color-p board x y *WHITE*) (is-field-color-p board x y *BLACK*)) t NIL)
  )

(defun is-field-empty (board x y)
  (if (is-field-color-p board x y *EMPTY*) t nil)
  )

;;
;;
;;
(defparameter *DIRECTIONS* '((0 1) (1 0) (1 1) (1 -1)))

(defun max-line-length-at (board x y color)
    (let ( (all '()))
      (dolist (d *DIRECTIONS*)
	(push (line-length-at board x y (first d) (second d) color) all)
	)
      (apply #'max all)
    )
  )

;;;
;;;
;;;
(defun is-four (board x y)
  (let ((l (max-line-length-at board x y (get-field board x y))))
    (if (>= l 4) t nil))
  )

;;;
;;; Calculates the row into which a piece will fall if its thrown into the given column
;;; returns y or nil if all fields of the column are already occupied
;;;
(defun find-row (board x)
  (if (is-field-empty board x 0)
      (+ (line-length-at board x 0 0 1 *EMPTY*) -1)
    nil
    ))

(defun invert-color (color)
   (if (eq color *WHITE*) *BLACK* *WHITE*)
   )


;;
;; Board formatter
;;

(defclass cell-formatter ()
  ((highlight-cells :initarg highlight-cells :initform '())
   ))

(defgeneric format-horizontal-cell-margin(formatter)
  (:documentation "Format the horizontal space between two cells in a row"))

(defgeneric format-cell-value (formatter cell-value)
  (:documentation "Formats a single cell represented by its value"))

(defgeneric format-cell (formatter board x y)
  (:documentation "Formats a single cell represented by its position within the board"))

(defgeneric format-border-top (formatter x)
  (:documentation "Format the header of given column"))

(defgeneric format-border-bottom (formatter x)
  (:documentation "Format the footer of given column"))

(defgeneric format-border-left (formatter y)
  (:documentation "Format the left border of given row"))

(defgeneric format-border-right (formatter y)
  (:documentation "Format the right border of given row"))

;; Default impl
(defmethod format-cell-value ( (formatter cell-formatter) cell-value)
  (cond
   ((equal cell-value *WHITE*) "W")
   ((equal cell-value *BLACK*) "B")
   (t "_")
   ))

(defmethod format-horizontal-cell-margin( (formatter cell-formatter))
  " ")

(defmethod format-cell ( (formatter cell-formatter) board x y)
  (format-cell-value formatter (get-field board x y))
   )

(defmethod format-border-top ( (formatter cell-formatter) x)
  (format nil "~1,'0x" x)
   )
(defmethod format-border-bottom ( (formatter cell-formatter) x)
  (format nil "~1,'0x" x)
   )
(defmethod format-border-left ( (formatter cell-formatter) y)
  (format nil "~1,'0x" y)
   )
(defmethod format-border-right ( (formatter cell-formatter) y)
  (format nil "~1,'0x" y)
   )

(defclass colorful-cell-formatter (cell-formatter)
  ((highlight-cells :initarg highlight-cells :initform '())
   ))

;; override simple B/W formatting
(defmethod format-cell-value ( (formatter colorful-cell-formatter) cell-value)
  (cond
   ((equal cell-value *WHITE*) (format nil "~c[32mW~c[0m" #\Esc #\Esc))
   ((equal cell-value *BLACK*) (format nil "~c[31mB~c[0m" #\Esc #\Esc))
   (t "_")
   ))

;;
;; Pretty print a board using a formatter
;;
(defun format-board (board &optional cell-formatter )
  (if (not cell-formatter) (setf cell-formatter (make-instance 'cell-formatter)))
  (let ((inner-height (get-board-height board)) (inner-width (get-board-width board)))
    (let ( (formatted-board (make-array (list (+ 2 inner-height) (+ 2 inner-width)) :initial-element "X")))
      ;; format inner field
      (dotimes (y inner-height)
	(dotimes (x inner-width)
	  (setf (aref formatted-board (+ y 1) (+ x 1)) (format-cell cell-formatter board x y))
	  ))
      ;; format top/bottom border
      (dotimes (x inner-width)
	  (setf (aref formatted-board 0 (+ x 1)) (format-border-top cell-formatter x))
	  (setf (aref formatted-board (+ inner-height 1) (+ x 1)) (format-border-bottom cell-formatter x))
	  )
      ;; format left/right border
      (dotimes (y inner-height)
	  (setf (aref formatted-board (+ y 1) 0) (format-border-left cell-formatter y))
	  (setf (aref formatted-board (+ y 1) (+ inner-width 1)) (format-border-right cell-formatter y))
	  )
      ;; inner function that joins all strings of a given row
      (flet ((join-row (y)
		       (let ((result (aref formatted-board y 0)))
			 (dotimes (x (+ (array-dimension formatted-board 1) -1))
			   (setf result (concatenate 'string result (format-horizontal-cell-margin cell-formatter)))
			   (setf result (concatenate 'string result (aref formatted-board y (+ x 1))))
			   )
			 result)))
	    (dotimes (y (array-dimension formatted-board 0))
	      (princ (join-row y))
	      (princ #\newline))
	    )))
  nil
  )

