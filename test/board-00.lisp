

(in-package :connect4-test)


;;;
;;;
;;; Status: OK
;;;
;;; Next move by: WHITE
;;;
;;; This board presents a situation where WHITE will immediately win
;;;
(defun create-board-00 ()
  (create-test-board (list
		      "......."
		      "......."
		      "......."
		      "..w...."
		      "..wbbw."
		      ".bwbwww"
		      )))

;;; Test with traversal depth 1
(define-test test-board-00-a ()
	     (run-minmax-test 
	      "test-board-00-a" (create-board-00) connect4::WHITE 1
	      ;; :print-final-scores t
	      :expected-final-columns '(2)
	      ))

;;; Test with traversal depth 6 and disabled depth relative scores
;;; Multiple winning moves. The computer may not chose the direct win
(define-test test-board-00-b ()
	     (run-minmax-test 
	      "test-board-00-b" (create-board-00) connect4::WHITE 6
	      :expected-final-columns '(2 3 4 5) 
	      ;; :print-final-scores t
	      ))

;;; Test with traversal depth 6 and depth relative scoring
;;; Computer must chose direct win
(define-test test-board-00-c ()
	     (run-minmax-test 
	      "test-board-00-c" (create-board-00) connect4::WHITE 6
	      :expected-final-columns '(2) 
	      ;; :print-final-scores t
	      ))


