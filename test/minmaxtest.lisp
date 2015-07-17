


(load "testutil.lisp")
(load "../boardformatter.lisp")
(load "../classic.lisp")

(defun test-simple ()
  (let ((board
	 ;; next move of w should be into column 2
	(create-test-board (list
			    "......."
			    "......."
			    "......."
			    "..w...."
			    "b.w.b.w"))))
    (let ((*hook-is-four* t) (*hook-reducing* t))
      (best-move board *WHITE* 3)
      )))


(defun test-very-simple ()
  (format t "************* Lets Go *********************~%")
  (let ( (board nil) (best-move nil))
    (setf board (create-test-board (list
				    "...."
				    "...."
				    "..w."
				    "bbwb")))
    (setf best-move (best-move board *WHITE* 3)) ;; 0..3
    (format t "~%")
    (format-board board (make-instance 'colorful-cell-formatter))
    (format t "~%Best move is column ~a row ~a with score ~a~%" (first best-move) (second best-move) (third best-move))
    (format t "Expected column of best move is column 2~%")
    ))




(test-very-simple)



