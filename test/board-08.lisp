

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
	     (run-minmax-test 
	      "test-board-08-a" (create-board-08) connect4::WHITE 4
	      :expected-final-columns '(1)
	      ))

;;; BLACK must answer with 0.0 for 0 or 1 or 4 and with -1.0 for 2 and 3 
(define-test test-board-08-c ()
	     (run-minmax-test 
	      "test-board-08-c" (create-board-08) connect4::BLACK 4
	      :expected-final-columns '(0 1 4)
	      ))

