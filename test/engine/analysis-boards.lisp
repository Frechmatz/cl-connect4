
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
(defun create-board-analysis-01 ()
  (create-test-board (list
		      "......."
		      "......."
		      "...B..."
		      "...w.w."
		      "B.ww.w."
		      "B.bw.bb"
		      )))

;;; Test with traversal depth 6 BLACK
(define-test test-board-analysis-01-a ()
	     (run-minmax-test 
	      "test-board-analysis-01-a" (create-board-analysis-01) connect4::BLACK 6
	      :expected-final-column 2
	      ))







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



