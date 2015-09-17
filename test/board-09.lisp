



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
	     (run-minmax-test 
	      "test-board-09-a" (create-board-09) connect4::WHITE 4
	      :expected-final-columns '(0 1 2 3 4)
	      ;; :print-final-scores t
	      ))


;;; Check that win situation is detected with traversal depth of 6
(define-test test-board-09-b ()
	     (run-minmax-test 
	      "test-board-09-b" (create-board-09) connect4::WHITE 6
	      :expected-final-columns '(1)
	      ))
