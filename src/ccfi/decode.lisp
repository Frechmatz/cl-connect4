
(in-package :ccfi)

(define-condition invalid-field-definition-error (error)
  ((text :initarg :text :reader text)))

(defun field-token-p (str)
  (or (equal str "o") (equal str "x")))
  
(defun scan-row (str)
  (let ((items '()) (tokens (cl-ppcre:all-matches-as-strings "[ox]|\\d+" str)))
    (dolist (token tokens)
      (if (field-token-p token)
	  (push token items)
	  (handler-case
	      (push (parse-integer (format nil "~a" token) :radix 10) items)
	    (parse-error () (error 'invalid-field-definition-error :text (format nil "Not a number: ~a" token))))
      ))
    (nreverse items)))

(defun row-width (scanned-row)
  (let ((w 0))
    (dolist (element scanned-row)
      (if (field-token-p element)
	  (setf w (+ w 1))
	  (setf w (+ w element))))
    w))

(defun decode-board (ccfiStr createBoardFn setBoardFieldFn)
  "Create an array out of the ccfi representation of a board
createBoardFn (dx dy)
setBoardFieldFn (x y)
"
  (declare (ignore createBoardFn))
  (declare (ignore setBoardFieldFn))
  (let ((rows (cl-ppcre:split "/" ccfiStr)))
    (dolist (row rows)
      (format t "~%~a~%" row)
      (scan-row row)
      )))


