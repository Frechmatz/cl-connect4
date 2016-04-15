

(in-package :ccfi)


(defun play-result-formatter-move (result)
  (format nil "~a"
	  (if (engine:play-result-no-move-available result)
	      "NULL"
	      (engine:play-result-column result))))

(defun play-result-formatter-four (result)
  (if (engine:play-result-is-four-n result)
      (format nil "--four ~a"
	      (reduce
	       (lambda (a b) (concatenate 'string (format nil "~a" a) "/" (format nil "~a" b)))
	       (engine:play-result-players-move-sequence result)))
      ""))



(defun format-play-result (result)
  (format nil "bestmove ~a ~a"
	  (play-result-formatter-move result)
	  (play-result-formatter-four result)
	  ))


