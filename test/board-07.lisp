
(in-package :connect4-test)


;;;; 
;;;; Tests that
;;;; BLACK recognizes that it has lost and responds with a negative score for all columns
;;;; WHITE will choose move for direct win
;;;;
;;;; Depth: 2 half moves
;;;;

(defun create-board-07 ()
  (create-test-board (list
		      "....."
		      ".wWw."
		      )))

;;; BLACK must answer with negative value for all columns
(define-test test-board-07-a ()
	     (run-minmax-test 
	      "test-board-07-a" (create-board-07) connect4::BLACK 2
	      :expected-final-columns '(0 1 2 3 4)
	      ))

;;; WHITE must answer with 0 or 4
(define-test test-board-07-c ()
	     (run-minmax-test 
	      "test-board-07-c" (create-board-07) connect4::WHITE 2
	      :expected-final-columns '(4 0)
	      ))

