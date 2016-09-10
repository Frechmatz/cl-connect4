
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

;; Leave two (instead of one) columns un-occupied in order
;; to let the
;; reducer really do some work (by comparing scores).
;; Tests the sanity of the result, when during
;; recursion the situation occurs that the move generator
;; provides zero possible moves. In this case the recursion
;; 'bounces' and returns a value of nil.
;; The purpose of this test is, to test, that the 'bouncing'
;; happens and that it is properly handled.
;; For the given field the minimum depth is 3, otherwise
;; the recursion will never 'bounce' because at least one
;; move is always available.
;; A more complex test case:
;; play ooxxxo/xxox1o/oxxx1x/ooxo1o/1oxo1x o 6
(define-test test-no-move-available-3 ()
	     (let ((result
		    (engine:play
		     (create-test-board
		      (list
		       "..wbw"
		       "bbbwb"
		       "wbwbw"
		       "bwbwb" ))
		     board:WHITE
		     3)))
	       (assert-played-column result '(0 1))))
