

(in-package :connect4-test)

;;;;
;;;;
;;;; Status: In work. 
;;;; 

(define-test test-board-score-4 ()
  (let ( (board nil))
    (setf board (create-test-board (list
		      "......."
		      "...w..."
		      "...w..."
		      "...w..."
		      "...w..."
		      )))
    (assert-equal 1.0 (connect4::board-score board 3 2) (format t "test-board-score-4 failed"))
    ))

(define-test test-board-score-3 ()
	     (let ( (board nil) (score nil)
		   (connect4::*engine-configuration-score-calculation-considers-three* t)
		    )
    (setf board (create-test-board (list
		      "......."
		      "......."
		      "...w..."
		      "...w..."
		      "...w..."
		      )))
    (setf score (connect4::board-score board 3 2))
    (assert-equal 0.5 score (format t "test-board-score-3 failed. Score is ~a~%" score))
    ))



