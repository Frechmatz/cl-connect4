

(defparameter *WIDTH* 7)
(defparameter *HEIGHT* 6)

(defparameter *BLACK* 'B)
(defparameter *WHITE* 'W)
(defparameter *EMPTY* '_)
(defparameter *BORDER* 'X)

(defun create-board ()
  (let ( (board (make-array `( ,(+ 2 *HEIGHT*) ,(+ 2 *WIDTH*)) :initial-element *EMPTY*)))
    (dotimes (x (+ 2 *WIDTH*)) (setf (aref board 0 x) (if (= x 0) *BORDER* (- x 1))))
    (dotimes (x (+ 2 *WIDTH*)) (setf (aref board (+ 1 *HEIGHT*) x) (if (= x 0) *BORDER* (- x 1))))
    (dotimes (y (+ 2 *HEIGHT*)) (setf (aref board y 0) (if (= y 0) *BORDER* (- y 1))))
    (dotimes (y (+ 2 *HEIGHT*)) (setf (aref board y (+ 1 *WIDTH*)) (if (= y 0) *BORDER* (- y 1))))
    board
    ))

(defun clone-board (board)
  ;; Bozo algorithm :(
  (let ((new-board (create-board)))
    (dotimes (x (+ 2 *WIDTH*))
      (dotimes (y (+ 1 *HEIGHT*))
	(setf (aref new-board y x) (aref board y x))
	))
    new-board))



(defun get-field (board x y)
  (aref board (+ 1 y) (+ 1 x)))

(defun set-field (board x y color)
  (let ((new-board (clone-board board)))
    (setf (aref new-board (+ 1 y) (+ 1 x)) color)
    new-board
    ))


	

; Check if a field has a given color
(defun is-field-color-p (board x y color)
  (eq (get-field board x y) color)
  )


#|
    Calculate the total length of the line at given position and for given direction
    x y: Starting point from which adjacent points are checked for the same color
|#
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
    

(defun is-four (board x y)
  (if (not (or (is-field-color-p board x y *WHITE*) (is-field-color-p board x y *BLACK*))) NIL 
    (let ((found-direction nil)
	  (directions '((N_S 0 1) (W_E 1 0) (NW_SE 1 1) (SW_NE 1 -1))))
      (dolist (d directions)
	(if (>= (line-length-at board x y (second d) (third d)) 4)
	    (progn
	      (setf found-direction (first d))
	      (return))
	  )
	)
      found-direction)))




