

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
	     (run-minmax-test 
	      "test-board-06-a" (create-board-06) board:WHITE 1
	      ;; compare full final scores without quitting row evaluation on a 4
	      :expected-final-columns '(5)
	      :is-mate-expected-for-player 1
	      ))

;;; No winning or lose position
(define-test test-board-06-b ()
	     (run-minmax-test 
	      "test-board-06-b" (create-board-06) board:BLACK 1
	      :expected-final-columns '(0 1 2 3 4 5 6)
	      ;; :print-final-scores t
	      :is-mate-expected-for-player 0
	      ))

;;; BLACK counters the threat
(define-test test-board-06-c ()
	     (run-minmax-test 
	      "test-board-06-c" (create-board-06) board:BLACK 2
	      :expected-final-columns '(5)
	      :is-mate-expected-for-player 0
	      ))


;;; Winning position at 5. Exit row evaluation on a 4
(define-test test-board-06-e ()
	     (run-minmax-test 
	      "test-board-06-e" (create-board-06) board:WHITE 1
	      :expected-final-columns '(5)
	      :is-mate-expected-for-player 1
	      ))
