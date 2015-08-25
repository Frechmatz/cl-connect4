

(in-package :connect4-test)

(define-test test-board-score-4 ()
  (let ( (board nil) (connect4::*classic-skip-randomizer* t))
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
  (let ( (board nil) (connect4::*classic-skip-randomizer* t))
    (setf board (create-test-board (list
		      "......."
		      "......."
		      "...w..."
		      "...w..."
		      "...w..."
		      )))
    (assert-equal 0.5 (connect4::board-score board 3 2) (format t "test-board-score-3 failed"))
    ))


