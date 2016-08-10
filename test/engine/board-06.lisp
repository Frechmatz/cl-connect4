

(in-package :connect4-test)

;;;;
;;;; Tests direct win for WHITE for column 5
;;;; Test that BLACK sees no winnign position (depth = 1)
;;;; Tests that BLACK counters with column 5 (depth = 2)
;;;;

(defun create-board-06 ()
  (create-test-board (list
		      "......."
		      "......."
		      ".Bbww.w"
		      )))


;;; Winning position at 5
(define-test test-board-06-a ()
	     (let ((result
		    (engine:play
		     (create-board-06) board:WHITE 1)))
	       (assert-played-column result '(5))
	       (assert-is-mate result)))

;;; No winning or lose position
(define-test test-board-06-b ()
	     (let ((result
		    (engine:play
		     (create-board-06) board:BLACK 1)))
	       (assert-played-column result '(0 1 2 3 4 5 6))
	       (assert-is-not-mate result)))

;;; BLACK counters the threat
(define-test test-board-06-c ()
	     (let ((result
		    (engine:play
		     (create-board-06) board:BLACK 2)))
	       (assert-played-column result '(5))
	       (assert-is-not-mate result)))

;;; Winning position at 5. Exit row evaluation on a 4
(define-test test-board-06-e ()
	     (let ((result
		    (engine:play
		     (create-board-06) board:WHITE 1)))
	       (assert-played-column result '(5))
	       (assert-is-mate result)))
