

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
	      :expected-final-column 2
	      ))

;;; Test with traversal depth 6
(define-test test-board-00-b ()
	     (run-minmax-test 
	      "test-board-00-b" (create-board-00) connect4::WHITE 6
	      :expected-final-column 2
	      ))
