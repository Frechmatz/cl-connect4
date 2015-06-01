

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
Returns a list of (x y) coordinates for all possible moves
board: The board
|#
(defun move-generator (board)
  )




