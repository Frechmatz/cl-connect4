 
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
    (let ((board (board:create-board width (length rows))) (row nil))
      (dotimes (y (length rows))
	(setf row (nth y rows))
	(if (not (equal (length row) width)) (error 'invalid-arguments :text "all rows must have same length"))
	(dotimes (x (length row))
	  (cond
	   ((or (equal (aref row x) #\W) (equal (aref row x) #\w)) (board:nset-field board x y board:WHITE))
	   ((or (equal (aref row x) #\B) (equal (aref row x) #\b)) (board:nset-field board x y board:BLACK))
	   )))
      board)))

(defun sort-scores-by-column (scores)
  (sort scores (lambda (a b) (< (first a) (first b)))))

(defun equal-scores-p (a b)
  (equalp (sort-scores-by-column a) (sort-scores-by-column b)))

(defun other-columns (columns board-width)
  (let ((result '()))
    (dotimes (current-column board-width)
      (if (not (find-if (lambda (c) (equal c current-column)) columns))
	  (push current-column result))
      ) result))

(defun run-minmax-test (name-of-test board color depth
			&key 
			  (print-final-scores nil)
			  (print-all-scores nil)
			  (expected-final-scores nil)
			  (expected-final-columns nil)
			  (expected-final-move-score nil)
			  (is-mate-expected-for-player nil))
  ;; (format t "Running minmax test ~a~%" name-of-test)
  (let ( (best-move nil)
	 (engine:*engine-notification-reduced-scores*
	  (lambda (board color is-opponent depth reduced-score all-scores)
	    (declare (ignore board))
	    (if (or (and print-final-scores (equal depth 1)) print-all-scores)
		(progn
		  (format t
			  "~%~a: Reduced scores: Depth: ~a Color: ~a Is-Opponent: ~a Score: ~a All scores:~%~a~%"
			  name-of-test depth color is-opponent (third reduced-score) all-scores)))
	    (if (and expected-final-scores (equal depth 1))
		(assert-true (equal-scores-p
			      all-scores
			      expected-final-scores)
			     (format t "~a: Final scores do not match. Expected:~%~a~%Resulting:~%~a~%"
				     name-of-test expected-final-scores all-scores))))))
    (setf best-move (engine:play board color depth))
    (if expected-final-columns
	(progn
	  (assert-true
	   (find-if (lambda (c) (equal c (play-result-column best-move))) expected-final-columns)
	   (format t "~a: Wrong move chosen: ~a. Score: ~a Expected move: ~a~%"
		   name-of-test
		   (play-result-column best-move)
		   (play-result-score best-move)
		   expected-final-columns))))
    (if expected-final-move-score
	(assert-true
	 ;; Regarding the comparison of floats, see also
	 ;; http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node74.html
	 (= expected-final-move-score (play-result-score best-move))
	 (format t "~a: Unexpected final score value: ~a Expected score value: ~a~%"
		 name-of-test
		 (play-result-score best-move)
		 expected-final-move-score)))
    (if (eq is-mate-expected-for-player 0)
	(assert-true (not (play-result-is-four-n best-move)) (format nil "Must not be mate")))
    (if (eq is-mate-expected-for-player 1)
	(assert-true (play-result-is-four-n best-move) (format t "~a: Must be mate~%" name-of-test)))
    best-move))



