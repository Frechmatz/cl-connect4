 
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
	  (push current-column result)))
    result))

;; see https://github.com/OdonataResearchLLC/lisp-unit/issues/36
(defun lisp-unit-or-wrapper (a b)
  (or a b))

(defmacro assert-played-column (cur-playresult allowed-columns)
  `(assert-true (lisp-unit-or-wrapper
		 (and (not ,allowed-columns) (not (playresult:play-result-column ,cur-playresult)))
		 (find-if (lambda (c) (equal c (playresult:play-result-column ,cur-playresult))) ,allowed-columns))))

(defmacro assert-is-mate (cur-playresult)
  `(assert-true (playresult:play-result-is-four-n ,cur-playresult)))
 
(defmacro assert-is-not-mate (cur-playresult)
  `(assert-true (not (playresult:play-result-is-four-n ,cur-playresult))))

