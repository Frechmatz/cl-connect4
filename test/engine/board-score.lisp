

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

(define-test test-board-score-iterator ()
	     (let ((board nil))
	       (setf board (create-test-board (list
					       "......."
					       "...w..."
					       "...w..."
					       "...w..."
					       "...w..."
					       )))
	       (let ((count 0))
		 (engine::for-each-board-field
		  board
		  board::WHITE
		  (lambda (x y) (setf count (+ 1 count))))
		 (assert-equal 4 count))))

(define-test test-board-score-field-variance ()
	     (let ((board nil))
	       (setf board (create-test-board (list
					       "......."
					       "...w..."
					       "...w..."
					       "...w..."
					       "...w..."
					       )))
	       (let ((v (engine::get-field-variance board 3 1 board::WHITE)))
		 (assert-equal 22 v))))

(define-test test-board-score-field-variances ()
	     (let ((board nil))
	       (setf board (create-test-board (list
					       "......."
					       "...w..."
					       "...w..."
					       "...w..."
					       "...w..."
					       )))
	       (let ((v (engine::get-field-variances board board::WHITE)))
		 (format t "Variances: ~a~%" v)
		 (assert-equal 4 (length v)))))


(define-test test-board-score-evaluate-board ()
	     (let ((board nil))
	       (setf board (create-test-board (list
					       "......."
					       "...w..."
					       "...w..."
					       "...w..."
					       "...w..."
					       )))
	       (let ((v (engine::get-board-variance board board::WHITE)))
		 (assert-true (> v 0)))))
