

#|
Implementation of the classic 4-Connect game
|#

#|
Evaluate the score of the board
x y: The latest move
|#
(defun board-score (board x y)
  (let ((length (max-line-length-at board x y)))
    (/ length *MAX-LENGTH*)
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
    (dolist (move moves)
      (setf x (first move))
      (setf y (second move))
      (setf cur-board (set-field board x y color))
      (push (list x y (board-score cur-board x y)) scores)
      )
    (max-list-value scores (lambda (x) (third x)))
    ))

