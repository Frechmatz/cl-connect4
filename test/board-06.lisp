

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
	      ;; compare full final scores without quitting row evaluation on a 4
	      :expected-final-columns '(5)
	      ))

;;; No winning or lose position
(define-test test-board-06-b ()
	     (run-minmax-test 
	      "test-board-06-b" (create-board-06) connect4::BLACK 1
	      :expected-final-columns '(0 1 2 3 4 5 6)
	      ;; :print-final-scores t
	      ))

;;; BLACK counters the threat
;;; depth relative score off
(define-test test-board-06-c ()
	     (run-minmax-test 
	      "test-board-06-c" (create-board-06) connect4::BLACK 2
	      :expected-final-columns '(5)
	      ))

;;; BLACK counters the threat
;;; depth relative score off
(define-test test-board-06-d ()
	     (run-minmax-test 
	      "test-board-06-d" (create-board-06) connect4::BLACK 2
	      :expected-final-columns '(5)
	      ))

;;; Winning position at 5. Exit row evaluation on a 4
(define-test test-board-06-e ()
	     (run-minmax-test 
	      "test-board-06-e" (create-board-06) connect4::WHITE 1
	      :expected-final-columns '(5)
	      ))
