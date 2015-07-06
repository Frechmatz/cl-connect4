
(load "testutil.lisp")


(defun test1 ()
  (let ((board
	(create-test-board (list
			    "......."
			    "......."
			    "......."
			    "B....BB"
			    "WbWwWBw"))))
    (format-board board)
    ))

(defun test2 ()
  (let ((board
	(create-test-board (list
			    "......."
			    "......."
			    "......."
			    "B....BB"
			    "WbWwWBw"))))
    (format-board board (make-instance 'colorful-cell-formatter))
    ))
