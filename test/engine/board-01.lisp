
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
	     (let ((result
		    (engine:play
		     (create-board-01) board:WHITE 4)))
	       (assert-played-column result '(0 1 2 3 4 5 6))))

;;; Let computer counter the the threat with a traversal depth of 6 half-moves
(define-test test-board-01-b ()
	     (let ((result
		    (engine:play
		     (create-board-01) board:WHITE 6)))
	       (assert-played-column result '(3 4 5))))

