
(in-package :connect4-test)

(define-condition invalid-arguments (error)
  ((text :initarg :text :reader text)))

;;
;; create a pre-set test-board
;; rows: list of strings representing the fields
;; for example ( "_____", "BWBWB"))
;; rows must be of same length
(defun create-test-board (rows)
  (if (not (listp rows)) (error 'invalid-arguments :text "rows argument must be a list"))
  (if (< (length rows) 2) (error 'invalid-arguments :text "minimum number of rows is 2"))
  (let ((width (length (car rows))))
    (if (< width 2) (error 'invalid-arguments :text "minimum length of a row is 2"))
    (let ((board (connect4::create-board width (length rows))) (row nil))
      (dotimes (y (length rows))
	(setf row (nth y rows))
	(if (not (equal (length row) width)) (error 'invalid-arguments :text "all rows must have same length"))
	(dotimes (x (length row))
	  (cond
	   ((or (equal (aref row x) #\W) (equal (aref row x) #\w)) (connect4::nset-field board x y connect4::WHITE))
	   ((or (equal (aref row x) #\B) (equal (aref row x) #\b)) (connect4::nset-field board x y connect4::BLACK))
	   )))
      board)))

(defun equal-scores-p (a b)
  (equalp a b))

(defun run-minmax-test (name-of-test board color depth
			&key 
			  (engine-configuration-skip-randomizer t)
			  (engine-configuration-depth-relative-score nil)
			  (print-final-scores nil)
			  (expected-final-scores nil)
			  (expected-final-column nil)
			)
  (let ( (best-move nil)
	(connect4::*engine-configuration-skip-randomizer* engine-configuration-skip-randomizer)
	 (connect4::*engine-configuration-depth-relative-score* engine-configuration-depth-relative-score)
	 (connect4::*engine-notification-reduced-scores*
	  (lambda (board color is-opponent depth reduced-score all-scores)
	    (declare (ignore board))
	    (if (and print-final-scores (equal depth 1))
		(progn
		  (format t
			  "~%~a: Final scores: Color: ~a Is-Opponent: ~a Depth: ~a Score: ~a All scores:~%~a~%"
			  name-of-test color is-opponent depth (third reduced-score) all-scores)
		  )
		)
	    (if (and expected-final-scores (equal depth 1))
		(assert-true (equal-scores-p
			      all-scores
			      expected-final-scores)
			     (format t "~a: Final scores do not match. Expected:~%~a~%Resulting:~%~a~%"
				     name-of-test expected-final-scores all-scores)
			     )
			   ) ; endif
	    ))
	 )
    (setf best-move (connect4::minmax board color depth))
    (if expected-final-column
	(assert-true
	 (equal expected-final-column (first best-move))
	 (format t "~a: Wrong move chosen: ~a. Score: ~a Expected move: ~a~%" name-of-test (first best-move) (third best-move) expected-final-column)
	 ))
    ))



