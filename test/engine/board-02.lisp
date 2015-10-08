

(in-package :connect4-test)

;;;;
;;;; Next move by: WHITE
;;;;
;;;; This board presents a situation where WHITE will in two moves
;;;; by throwing into column 4
;;;; Requires a traversal depth >= 3 half-moves
;;;;

(defun create-board-02 ()
  (create-test-board (list
		      "wb...."
		      "bbww.w"
		      "bbww.w"
		      "bwbb.."
		      )))

;;; test with traversal depth 2 (win situation won't be recognized)
(define-test test-board-02-a ()
	     (run-minmax-test 
	      "test-board-02-a" (create-board-02) board:WHITE 2
	      :expected-final-columns '(2 3 4 5)
	      ;; :expected-final-move-score 0.0
	      ))

;;; test with traversal depth 3 (win situation will be recognized)
(define-test test-board-02-b ()
	     (run-minmax-test 
	      "test-board-02-b" (create-board-02) board:WHITE 3
	      :expected-final-columns '(4)
	      ))
