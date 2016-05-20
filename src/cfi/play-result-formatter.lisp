

(in-package :cfi)

(defun play-result-formatter-move (result)
  (format nil "~a"
	  (if (playresult:play-result-no-move-available result)
	      "NULL"
	      (playresult:play-result-column result))))

(defun play-result-formatter-four (result)
  (if (playresult:play-result-is-four-n result)
      (format nil "--four ~a"
	      (reduce
	       (lambda (a b) (concatenate
			      'string
			      (format nil "~a" a)
			      "/"
			      (format nil "~a" b)))
	       (playresult:play-result-players-move-sequence result)))
      ""))

(defun get-line (board x y color)
  (mapcar
   (lambda (i)
     (list (first i) (- (board:get-height board) (second i) 1)))
   (board:get-connected-pieces
    (board:set-field
     board
     x
     y
     color)
    x y)))
  
(defun play-result-formatter-line (board result)
  (if (playresult:play-result-is-four-1 result)
      (format nil "--line ~a"
	      (reduce
	       (lambda (a b) (concatenate 'string a "/" b))
	       (mapcar
		(lambda (i) (concatenate
			     'string
			     (format nil "~a" (first i))
			     ";"
			     (format nil "~a" (second i))))
		(get-line
		 board
		 (playresult:play-result-column result)
		 (playresult:play-result-row result)
		 (playresult:play-result-players-color result)))))
      ""))


(defun format-play-result (board result)
  (format nil "bestmove ~a ~a ~a"
	  (play-result-formatter-move result)
	  (play-result-formatter-four result)
	  (play-result-formatter-line board result)
	  ))


