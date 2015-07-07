
(load "testutil.lisp")


(defun test-bw ()
  (let ((board
	(create-test-board (list
			    "......."
			    "......."
			    "......."
			    "B....BB"
			    "WbWwWBw"))))
    (format-board board)
    ))

(defun test-color ()
  (let ((board
	(create-test-board (list
			    "......."
			    "......."
			    "......."
			    "B....BB"
			    "WbWwWBw"))))
    (format-board board (make-instance 'colorful-cell-formatter))
    ))

(defun test-huge ()
  (let ((board
	(create-test-board (list
			    "................"
			    "................"
			    "................"
			    "................"
			    "................"
			    "................"
			    "................"
			    "................"
			    "................"
			    "................"
			    "................"
			    "B.............BB"
			    "WbWw.........WBw"))))
    (format-board board (make-instance 'colorful-cell-formatter))
    ))
