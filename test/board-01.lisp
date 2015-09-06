
(in-package :connect4-test)


;;;
;;;
;;; Status: Ok
;;;
;;; Next move by: WHITE
;;; WHITE must throw into 3 otherwise it will lose.
;;; Required traversal depth: > 4 half moves
;;;
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
	      :expected-final-column 0
	      ))

;;; Let computer counter the the threat with a traversal depth of 6 half-moves
(define-test test-board-01-b ()
	     (run-minmax-test 
	      "test-board-01-b" (create-board-01) connect4::WHITE 6
	      :expected-final-column 3
	      ))

