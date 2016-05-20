
;;;;
;;;; Call minmax with full board (->should not crash)
;;;;

(in-package :connect4-test)

(define-test test-no-move-available ()
	     (run-minmax-test 
	      "test-no-move-available"
	      (create-test-board (list
		      "wbwbw"
		      "bwbwb"
		      "wbwbw"
		      "bwbwb"
		      ))
	      board:WHITE 6
	      ;; :print-final-scores t
	      :expected-final-columns '()
	      ))



