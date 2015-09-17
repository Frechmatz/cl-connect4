
(in-package :connect4-test)


;;;;
;;;;
;;;; Next move by: WHITE
;;;; WHITE must throw into 3 or 4 or 5 otherwise it will lose.
;;;; Required traversal depth: > 4 half moves
;;;;

(defun create-board-01 ()
  (create-test-board (list
		      "......."
		      "......."
		      "..w...."
		      "..b...."
		      ".wbbbw."
		      "wbbbwww"
		      )))

;;; Let computer lose with a traversal depth of 4 half-moves
(define-test test-board-01-a ()
	     (run-minmax-test 
	      "test-board-01-a" (create-board-01) connect4::WHITE 4
	      :expected-final-columns '(0 1 2 3 4 5 6)
	      ;;:print-final-scores t
	      ;;:expected-final-move-score 0.0
	      ))

;;; Let computer counter the the threat with a traversal depth of 6 half-moves
(define-test test-board-01-b ()
	     (run-minmax-test 
	      "test-board-01-b" (create-board-01) connect4::WHITE 6
	      :expected-final-columns '(3 4 5)
	      ;; :print-final-scores t
	      ))

