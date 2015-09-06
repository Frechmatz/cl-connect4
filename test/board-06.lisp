

(in-package :connect4-test)

;;;
;;; Tests direct win for WHITE for column 5
;;; Test that BLACK sees no winnign position (depth = 1)
;;; Tests that BLACK counters with column 5 (depth = 2)
;;;
;;; Status: Ok
;;;

(defun create-board-06 ()
  (create-test-board (list
		      "......."
		      "......."
		      ".Bbww.w"
		      )))


;;; Winning position at 5
(define-test test-board-06-a ()
	     (run-minmax-test 
	      "test-board-06-a" (create-board-06) connect4::WHITE 1
	      :expected-final-scores
	      '((0 2 0.0) (1 1 0.0) (2 1 0.0) (3 1 0.0) (4 1 0.0) (5 2 1.0) (6 1 0.0))
	      :expected-final-column 5
	      ))

;;; No winning or lose position
(define-test test-board-06-b ()
	     (run-minmax-test 
	      "test-board-06-b" (create-board-06) connect4::BLACK 1
	      :expected-final-scores
	      '((0 2 0.0) (1 1 0.0) (2 1 0.0) (3 1 0.0) (4 1 0.0) (5 2 0.0) (6 1 0.0))
	      :expected-final-column 0
	      ))

;;; BLACK counters the threat
(define-test test-board-06-c ()
	     (run-minmax-test 
	      "test-board-06-c" (create-board-06) connect4::BLACK 2
	      :expected-final-scores
	      '((0 2 -1.0) (1 1 -1.0) (2 1 -1.0) (3 1 -1.0) (4 1 -1.0) (5 2 0.0) (6 1 -1.0))
	      :expected-final-column 5
	      ))
