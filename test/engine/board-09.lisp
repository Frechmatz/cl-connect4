



;;;;
;;;; Test check mate in 6 half moves
;;;; WHITE must realize its win
;;;;

(in-package :connect4-test)


;;;
;;; Depth: 6 half moves
;;; Winning column for WHITE is 1
;;;

(defun create-board-09 ()
  (create-test-board (list
		      "..b.."
		      "w.ww."
		      "w.ww."
		      "b.wb."
		      "b.bw."
		      )))

;;; Check that win situation won't be detected with traversal depth of 4
(define-test test-board-09-a ()
	     (let ((result
		    (engine:play
		     (create-board-09) board:WHITE 4)))
	       (assert-played-column result '(0 1 2 3 4))
	       (assert-is-not-mate result)))

;;; Check that win situation is detected with traversal depth of 6
(define-test test-board-09-b ()
	     (let ((result
		    (engine:play
		     (create-board-09) board:WHITE 6)))
	       (assert-played-column result '(1))
	       (assert-is-mate result)))
