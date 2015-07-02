

#|
Common Connect4 field related functions as creating, cloning and some helpers
|#


(defparameter *BLACK* 'B)
(defparameter *WHITE* 'W)
(defparameter *EMPTY* '_)
(defparameter *BORDER* 'X)

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

(defun clone-board (board)
  ;; Bozo algorithm :(
  (let ((new-board (create-board (get-board-width board) (get-board-height board))))
    (dotimes (x (+ 2 (get-board-width board)))
      (dotimes (y (+ 1 (get-board-height board)))
	(setf (aref new-board y x) (aref board y x))
	))
    new-board))

(defun get-board-width (board)
  (+ (array-dimension board 1) -2))

(defun get-max-x (board)
  (+  (get-board-width board) -1))

(defun get-board-height (board)
  (+ (array-dimension board 0) -2))

(defun get-max-y (board)
  (+  (get-board-height board) -1))

(defun get-field (board x y)
  (aref board (+ 1 y) (+ 1 x)))

(defun set-field (board x y color)
  (let ((new-board (clone-board board)))
    (setf (aref new-board (+ 1 y) (+ 1 x)) color)
    new-board
    ))

(defun nset-field (board x y color)
  (setf (aref board (+ 1 y) (+ 1 x)) color)
  board
    )

;; Check if a field has a given color
(defun is-field-color-p (board x y color)
  (eq (get-field board x y) color)
  )


;;;
;;; Calculate the total length of the line at the given position and for given direction
;;; x y: Starting point from which adjacent points are checked for the same color
;;; 
(defun line-length-at (board x y dx dy)
  (let ((length 0) (color (get-field board x y))) 
    (flet (
	   (go (dx dy)
	       (do
		((curX x (+ curX dx)) (curY y (+ curY dy)))
		((not (is-field-color-p board curX curY color)))
		(setf length (+ length 1))
		)))
	  ; Body of flet
	  (go dx dy) ; go forward
	  (go (* -1 dx) (* -1 dy)) ; go backward
	  (- length 1) ; start position has been accounted for two times
	  )))

(defun is-field-set (board x y)
  (if (or (is-field-color-p board x y *WHITE*) (is-field-color-p board x y *BLACK*)) t NIL)
  )


(defun max-line-length-at (board x y)
  (if (not (is-field-set board x y)) 0
    (let (
	  (all '())
	  (directions '((0 1) (1 0) (1 1) (1 -1)))
	  )
      (dolist (d directions)
	(push (line-length-at board x y (first d) (second d)) all)
	)
      (apply #'max all)
    )
    )
  )


;;;
;;;
;;;
(defun is-four (board x y)
  (let ((l (max-line-length-at board x y)))
    (if (>= l 4) t nil))
  )

;;;
;;; Calculates the row into which a piece will fall if its thrown into the given column
;;; returns y or nil if all fields of the column are already occupied
;;;
(defun find-row (board x)
  (if (is-field-set board x 0)
      nil
    (+ (line-length-at board x 0 0 1) -1)
    ))

(defun invert-color (color)
   (if (eq color *WHITE*) *BLACK* *WHITE*)
   )


;;
;; Board formatter
;;

(defclass cell-formats ()
  ((highlight-cells :initarg highlight-cells :initform '())
   ))

(defmethod format-cell-value ( (formats cell-formats) cell-value)
  cell-value
  )

(defmethod format-cell ( (formats cell-formats) board x y)
  (format-cell-value (get-field board x y))
   )

(defclass colorful-cell-formats (cell-formats)
  ((highlight-cells :initarg highlight-cells :initform '())
   ))

(defmethod format-cell-value ( (formats colorful-cell-formats) cell-value)
  (cond
   ((equal cell-value *WHITE*) (format nil "~c[32mW~c[0m" #\Esc #\Esc))
   ((equal cell-value *BLACK*) (format nil "~c[31mB~c[0m" #\Esc #\Esc))
   (t cell-value)
   ))

(defmethod format-cell ( (formats colorful-cell-formats) board x y)
  (format-cell-value formats (get-field board x y))
  )


(defun format-board (board &optional cell-formats )
  (if (not cell-formats) (setf cell-formats (make-instance 'cell-formats)))
  (let ((rows '()) (footer '()) (header '() ))
    (push *BORDER* header)
    (push *BORDER* footer)
    (dotimes (x (get-board-width board))
      (push x header)
      (push x footer))
    (push *BORDER* header)
    (push *BORDER* footer)
    (push (nreverse header) rows)
    (dotimes (y (get-board-height board))
      (let ((row '()))
	(push *BORDER* row)
	(dotimes (x (get-board-width board))
	  (push (format-cell cell-formats board x y) row)
	  )
	(push *BORDER* row)
	(push (nreverse row) rows)
	))
    (push (nreverse footer) rows)
    (dolist (l (nreverse rows))
      (princ l)
      (princ #\newline)
      )
    nil))


