

(in-package :cfi)

(defun play-result-formatter-move (result)
  (format nil "~a"
	  (if (engine:play-result-no-move-available result)
	      "NULL"
	      (engine:play-result-column result))))

(defun play-result-formatter-four (result)
  (if (engine:play-result-is-four-n result)
      (format nil "--four ~a"
	      (reduce
	       (lambda (a b) (concatenate 'string (write-to-string a) "/" (write-to-string b)))
	       (engine:play-result-players-move-sequence result)))
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
  (if (engine:play-result-is-four-1 result)
      (format nil "--line ~a"
	      (reduce
	       (lambda (a b) (concatenate 'string a "/" b))
	       (mapcar
		(lambda (i) (concatenate 'string (write-to-string (first i)) ";" (write-to-string (second i))))
		(get-line
		 board
		 (engine:play-result-column result)
		 (engine:play-result-row result)
		 (engine:play-result-players-color result)))))
      ""))


(defun format-play-result (board result)
  (format nil "bestmove ~a ~a ~a"
	  (play-result-formatter-move result)
	  (play-result-formatter-four result)
	  (play-result-formatter-line board result)
	  ))


