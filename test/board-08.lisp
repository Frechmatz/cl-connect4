

;;;
;;; Test check mate in 4 half moves
;;; WHITE must realize its win
;;; BLACK must counter the threat
;;;
;;; Status: Ok
;;;

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
	      :expected-final-scores
	      '((0 1 0.0) (1 1 1.0) (2 0 0.0) (3 0 0.0) (4 1 0.0))
	      :expected-final-column 1
	      :engine-configuration-depth-relative-score nil
	      ))

;;; WHITE must answer with 1.0 for 1
;;; Depth relative scores on
(define-test test-board-08-b ()
	     (run-minmax-test 
	      "test-board-08-b" (create-board-08) connect4::WHITE 4
	      :expected-final-column 1
	      ))

;;; BLACK must answer with 0.0 for 0 or 1 or 4 and with -1.0 for 2 and 3 
(define-test test-board-08-c ()
	     (run-minmax-test 
	      "test-board-08-c" (create-board-08) connect4::BLACK 4
	      :engine-configuration-depth-relative-score nil
	      :expected-final-scores
	      '((0 1 0.0) (1 1 0.0) (2 0 -1.0) (3 0 -1.0) (4 1 0.0))
	      ))


;;; BLACK must answer with 0.0 for 0 or 1 or 4 and with -1.0 for 2 and 3 
;;; Depth relative scores on
(define-test test-board-08-d ()
	     (run-minmax-test 
	      "test-board-08-d" (create-board-08) connect4::BLACK 4
	      :expected-final-column '(0 1 4)
	      ))

