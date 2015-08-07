
(load "testutil.lisp")
(load "../boardformatter.lisp")

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

(defun test-is-highlight-cell()
  (let ((hi-cells (list '(0 3))))
    (format t "Should be true: ~a~%"  (is-highlight-cell 0 3 hi-cells))
      ))

 (defun test-highlight-cells ()
  (let ( (hilight-cells nil) (board
	(create-test-board (list
			    "......."
			    "......."
			    "......."
			    "B....BB"
			    "WbWwWBw"))))
    (setf hilight-cells (list '(0 3) '(2 4)))
    (format-board board (make-instance 'colorful-cell-formatter) hilight-cells)
    ))
