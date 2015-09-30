

(in-package :connect4-test)

;;;;
;;;; Test board-score calculation 
;;;;

(define-test test-board-score-3 ()
	     (let ( (board nil) (score nil)
		    (connect4::*column-weights* (make-array 7 :initial-element 1.0)) 
		    )
	       (setf board (create-test-board (list
					       "......."
					       "......."
					       "...w..."
					       "...w..."
					       "...w..."
					       )))
	       (setf score (connect4::board-score board 3 2))
	       (assert-true (>= score 0.5) (format t "test-board-score-3 failed. Score is ~a~%" score))
	       ))


(define-test test-board-score-4 ()
	     (let ( (board nil)
		   (connect4::*column-weights* (make-array 7 :initial-element 1.0)) 
		    )
	       (setf board (create-test-board (list
					       "......."
					       "...w..."
					       "...w..."
					       "...w..."
					       "...w..."
					       )))
	       (assert-equal 1.0 (connect4::board-score board 3 2) (format t "test-board-score-4 failed"))
	       ))

