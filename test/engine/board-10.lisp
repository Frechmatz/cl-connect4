


;;;;
;;;; Some basic 3 pieces in a row related tests  
;;;;

(in-package :connect4-test)

;;; Basic test prefering the center of the board
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
	       (assert-played-column result '(2))))

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
	       (assert-played-column result '(2))))

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


;;; Basic test prefering the center of the board (1 half-moves)
(define-test test-board-10-f ()
	     (let ((result
		    (engine:play
		     (create-test-board (list
		      "......"
		      "......"
		      "......"
		      "......"
		      ))
		     board:WHITE 1)))
	       ;;(format t "XX Column: ~a~%" (engine::play-result-column result)) 
	       (assert-played-column result '(2 3))))
