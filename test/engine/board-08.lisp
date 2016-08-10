

;;;;
;;;; Test check mate in 4 half moves
;;;; WHITE must realize its win
;;;; BLACK must counter the threat
;;;;

(in-package :connect4-test)


;;;
;;; Depth: 4 half moves
;;;

(defun create-board-08 ()
  (create-test-board (list
		      "....."
		      "..wW."
		      )))

;;; WHITE must answer with 1.0 for 1
;;; Depth relative scores 
(define-test test-board-08-a ()
	     (let ((result
		    (engine:play
		     (create-board-08) board:WHITE 4)))
	       (assert-played-column result '(1))
	       (assert-is-mate result)))

;;; BLACK must answer with 0.0 for 0 or 1 or 4 and with -1.0 for 2 and 3 
(define-test test-board-08-c ()
	     (let ((result
		    (engine:play
		     (create-board-08) board:BLACK 4)))
		   (assert-played-column result '(0 1 4))
		   (assert-is-not-mate result)))


