


(in-package :connect4-test)


;;;
;;;
;;; Status: Review required. Not sure what the test tests at all.
;;;
;;; Next move by: BLACK
;;;
;;;
(defun create-board-03 ()
  (create-test-board (list
		      "......."
		      "..b..b."
		      ".bww.w."
		      ".wbb.ww"
		      ".bww.ww"
		      ".bbw.bb"
		      )))

;;; Test with traversal depth 6
(define-test test-board-03-a ()
	     (run-minmax-test 
	      "test-board-03-a" (create-board-03) connect4::BLACK 6
	      :expected-final-column 0
	      ))



