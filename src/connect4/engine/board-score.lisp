
(in-package :engine)



(defun for-each-board-field (board color fn)
  "Field iterator"
  (dotimes (x (board:get-width board))
    (dotimes (y (board:get-height board))
      (if (eq (board:get-field board x y) color)
	  (funcall fn x y)))))

(defun get-field-variance (board x y color)
  (reduce
   (lambda (a b) (+ a (length (second b))))
   (board:scan-board-all-directions
    board
    x
    y
    (lambda (c) (or (eq c color) (eq c board:empty))))
   :initial-value 0))

(defun get-field-variances (board color)
  (let ((variances nil))
    (for-each-board-field
     board
     color
     (lambda (x y)
       (push (get-field-variance board x y color) variances)))
    variances))

(defun normalize-total-variance (board variance)
  (/ variance (*
	       (board:get-width board)
	       (board:get-height board)
	       (board:get-width board)
	       (board:get-height board))))

	       
(defun get-board-variance (board color)
  "Calculate a board score"
  (normalize-total-variance
	    board
	    (reduce
	     (lambda (a b) (+ a b))
	     (get-field-variances board color)
	     :initial-value 0)))
