

#|
Implementation of the classic 4-Connect game
|#

#|
Evaluate the score of the board
x y: The latest move
|#
(defun board-score (board x y)
  (let ((length (max-line-length-at board x y)))
    (if (>= length 4) 1.0 (/ length *MAX-LENGTH*))
    ))

#|
Returns a list of (x y) coordinates of all possible moves
board: The board
|#
(defun generate-moves (board)
  (let ( (moves ()) (depth 0))
    (dotimes (x *WIDTH*)
      (if (not (is-field-set board x 0))
	  (progn
	    (setf depth (line-length-at board x 0 0 1))
	    (push (list x (- depth 1)) moves)
	    )
	)
      )
    moves
    ))

(defun best-move (board color)
  (let ((moves (generate-moves board)) (scores ()) (cur-board nil) (x nil) (y nil))
    ;;(print moves)
    (dolist (move moves)
      (setf x (first move))
      (setf y (second move))
      (push (list x y (get-minmax board x y color 1.0 1)) scores)
      )
    (max-list-value scores (lambda (x) (third x)))
    ))


(defun invert-color (color)
   (if (eq color *WHITE*) *BLACK* *WHITE*)
   )

(defun get-minmax (board x y color score-factor max-depth)
  (let ((moves nil) (score nil) (cur-board nil))
    ;; do current move
    (setf cur-board (set-field board x y color))
    (setf moves (generate-moves cur-board))
    ;; calculate score of current move in order to determine if we should calculate possible counter-moves
    (setf score (board-score cur-board x y))
    (print cur-board)
    (print color)
    (print score)
    (print (eq score 1.0))
    (if (or (equal score 1.0) (not moves) (eq max-depth 0)) (* score-factor score)
      (progn
	;; Maximize inverted responses of the opponent (Score range of board evaluator is 0..1, thats why we need to invert)
	(setf score -10000)
	(dolist (move moves)
	  (setf score (max score (get-minmax cur-board (first move) (second move) (invert-color color) (* -1.0 score-factor) (+ max-depth -1))))
	)  
	score
	))))

	
    
  
