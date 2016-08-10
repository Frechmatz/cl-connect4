


;;;;
;;;; Some basic 3 pieces in a row related tests  
;;;;

(in-package :connect4-test)

;;; Basic test not prefering the center of the board
(define-test test-board-10-a ()
	     (let ((result
		    (engine:play
		     (create-test-board (list
		      "....."
		      "....."
		      "....."
		      "....."
		      ))
		     board:WHITE 6)))
	       (assert-played-column result '(0 1 2 3 4))))

;;; Basic test of prefering the center of the board (1 half-move)
(define-test test-board-10-b ()
	     (let ((result
		    (engine:play
		     (create-test-board (list
		      "....."
		      "....."
		      "....."
		      "....."
		      ))
		     board:WHITE 1)))
	       (assert-played-column result '(2 3))))

;;; Basic test prefering the center of the board (2 half-moves)
(define-test test-board-10-c ()
	     (let ((result
		    (engine:play
		     (create-test-board (list
		      "....."
		      "....."
		      "....."
		      "....."
		      ))
		     board:WHITE 2)))
	       (assert-played-column result '(2 3))))

;;;
;;; WHITE: Realize three pieces in a row via 3 
;;;
(define-test test-board-10-d ()
	     (let ((result
		    (engine:play
		     (create-test-board (list
		      "....."
		      "....."
		      "...w."
		      "...w."
		      ))
		     board:WHITE 1)))
	       (assert-played-column result '(3))))

;;;
;;; BLACK: Hold off three pieces in a row for WHITE via 3 
;;;
(define-test test-board-10-e ()
	     (let ((result
		    (engine:play
		     (create-test-board (list
		      "....."
		      "....."
		      "...w."
		      "...w."
		      ))
		     board:BLACK 2)))
	       (assert-played-column result '(3))))

