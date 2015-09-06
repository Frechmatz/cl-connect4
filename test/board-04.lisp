
(in-package :connect4-test)

;;;
;;;
;;; Status: Review required. Not sure what the test tests at all. Not part of test suite.
;;;
;;; Next move by: BLACK
;;; 
;;; Computer must throw into column 2 otherwise
;;; WHITE will win via 2 - 4 - 4
;;;
(defun create-board-04 ()
  (create-test-board (list
		      "......."
		      "......."
		      "...B..."
		      "...w.w."
		      "B.ww.w."
		      "B.bw.bb"
		      )))

;;; Test with traversal depth 6 BLACK
(define-test test-board-04-a ()
	     (run-minmax-test 
	      "test-board-04-a" (create-board-04) connect4::BLACK 6
	      :expected-final-column 2
	      ))

;;; Test with traversal depth 6 WHITE
(define-test test-board-04-b ()
	     (run-minmax-test 
	      "test-board-04-b" (create-board-04) connect4::WHITE 6
	      :expected-final-column 2
	      ))
