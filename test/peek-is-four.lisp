
(in-package :connect4-test)

(define-test test-peek-is-four-a ()
	     (let (
		   (board (create-test-board (list
					      "....."
					      "....."
					      "....."
					      "....."
					      ))
		     )
		   (moves nil)
		   )
	       (setf moves (connect4::peek-is-four 
			    '((0 3) (1 3) (2 3) (3 3) (4 3))
			    board connect4::WHITE))
	       (assert-true (equalp moves '((0 3) (1 3) (2 3) (3 3) (4 3)))
			    (format t "test-peek-is-four-a has failed"))
	       ))

(define-test test-peek-is-four-b ()
	     (let (
		   (board (create-test-board (list
					      ".W..."
					      ".W..."
					      ".W..."
					      ".W..."
					      ))
		     )
		   (moves nil)
		   )
	       (setf moves (connect4::peek-is-four 
			    '((0 3) (1 0) (2 3) (3 3) (4 3))
			    board connect4::WHITE))
	       (assert-true (equalp moves '((1 0)))
			    (format t "test-peek-is-four-b has failed"))
	       ))

;;; Test empty list passed into function
(define-test test-peek-is-four-c ()
	     (let (
		   (board (create-test-board (list
					      ".W..."
					      ".W..."
					      ".W..."
					      ".W..."
					      ))
		     )
		   (moves nil)
		   )
	       (setf moves (connect4::peek-is-four 
			    '()
			    board connect4::WHITE))
	       (assert-true (equalp moves '())
			    (format t "test-peek-is-four-c has failed"))
	       ))
