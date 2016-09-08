

(in-package :connect4-test)

;;;;
;;;; Test board-score calculation 
;;;;


(define-test test-board-score-4 ()
	     (let ((board nil))
	       (setf board (create-test-board (list
					       "......."
					       "...w..."
					       "...w..."
					       "...w..."
					       "...w..."
					       )))
	       (assert-equal 1.0 (engine::board-score board 3 2))))

