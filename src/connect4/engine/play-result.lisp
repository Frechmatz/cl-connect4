
#|
Accessor functions for the result of the play() function
|#


(in-package :engine)

(defun play-result-is-line-four (line)
  (equal (third line) "MATE"))

(defun play-result-column (result)
  (first result))

(defun play-result-row (result)
  (second result))

(defun play-result-score (result)
  (third result))

(defun play-result-is-four-1 (result)
  (play-result-is-line-four (first (fourth result))))

(defun play-result-is-four-n (result)
  (let ((is-four nil))
    (dolist (move (fourth result))
	     (if (not is-four)
		 (setf is-four (play-result-is-line-four move))))
    is-four))

