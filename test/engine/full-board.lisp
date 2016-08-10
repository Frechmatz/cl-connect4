
;;;;
;;;; Call minmax with full board (->should not crash)
;;;;

(in-package :connect4-test)

(define-test test-no-move-available ()
	     (let ((result
		    (engine:play
		     (create-test-board
		      (list
		       "wbwbw"
		       "bwbwb"
		       "wbwbw"
		       "bwbwb"))
		     board:WHITE
		     6)))
	       (assert-played-column result '())))

(define-test test-no-move-available-2 ()
	     (let ((result
		    (engine:play
		     (create-test-board
		      (list
		       ".bwbw"
		       "bbbwb"
		       "wbwbw"
		       "bwbwb" ))
		     board:WHITE
		     6)))
	       (assert-played-column result '(0))))

