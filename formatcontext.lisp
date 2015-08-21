
(in-package :connect4)

(defun format-context (context &optional highlight-cells)
  "Print board and statuses"
    (format-board *board-formatter* (slot-value context 'board) highlight-cells)
    (format t "~%Wins: ~a Loses: ~a Draws: ~a"
	    (slot-value context 'wins)
	    (slot-value context 'loses)
	    (slot-value context 'draws))
    (format t "~%Level: ~a Your color: ~a~%"
	    (slot-value context 'difficulty-level)
	    (format-cell-value *board-formatter* (slot-value context 'players-color))))



