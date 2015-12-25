
(in-package :ccfi)

(define-condition invalid-field-definition-error (error)
  ((text :initarg :text :reader text)))

(defun field-token-p (str)
  (or (equal str "o") (equal str "x")))
  
(defun scan-row (str)
  ;; Empty row check
  (if (or (not str) (equal str ""))
      (error 'invalid-field-definition-error :text (format nil "Row must not be empty"))
      ;; Illegal character check
      (if (cl-ppcre:all-matches-as-strings "[^ox\\d]" str)
	  (error 'invalid-field-definition-error :text (format nil "Invalid characters in row: ~a" str))
	  ;; let's go
	  (let ((items '()) (tokens (cl-ppcre:all-matches-as-strings "[ox]|\\d+" str)))
	    (dolist (token tokens)
	      (if (field-token-p token)
		  (push token items)
		  (push (parse-integer token :radix 10) items)
		  ))
	  (nreverse items)))))

(defun process-parsed-row (setBoardFieldFn y scanned-row)
  (let ((x 0))
    (dolist (element scanned-row)
      (if (field-token-p element)
	  (progn
	    (funcall setBoardFieldFn x y element)
	    (setf x (+ x 1)))
	  (dotimes (col element)
	    (funcall setBoardFieldFn x y nil)
	    (setf x (+ x 1)))
	  ))))
		    
(defun row-width (scanned-row)
  (let ((w 0))
    (process-parsed-row
     (lambda (x y token)
       (declare (ignore x))
       (declare (ignore y))
       (declare (ignore token))
       (setf w (+ w 1))) 0 scanned-row)
    w))

(defun split-board-to-rows (ccfiStr)
  (if (or (not ccfiStr) (equal ccfiStr ""))
      (error 'invalid-field-definition-error :text (format nil "Board must not be empty"))
      (let ((rows (cl-ppcre:split "/" ccfiStr)))
	(if (not rows)
	    (error 'invalid-field-definition-error :text (format nil "Board must not be empty"))
	    rows))))

(defun decode-board-old (ccfiStr createBoardFn setBoardFieldFn)
  "Create an array out of the ccfi representation of a board
createBoardFn (dx dy)
setBoardFieldFn (x y)
"
  (declare (ignore setBoardFieldFn))
      (let ((rows (split-board-to-rows ccfiStr)))
	(let ((height (cl:length rows)) (width nil) (y 0))
	  (dolist (row rows)
	    (let ((scanned-row (scan-row row)))
	      (let ((cur-row-width (row-width scanned-row)))
		(if (not width)
		    (progn
		      (setf width cur-row-width)
		      (funcall createBoardFn width height))
		    (progn
		      (if (not (equal width cur-row-width))
			  (error 'invalid-field-definition-error :text "Rows must have same length")
			  (progn
			    (set-row setBoardFieldFn y scanned-row)
			    (setf y (+ 1 y)))
			  )))))))))


(defun decode-board (ccfiStr createBoardFn setBoardFieldFn)
  "Create an array out of the ccfi representation of a board
createBoardFn (dx dy)
setBoardFieldFn (x y)
"
  (let ((rows (split-board-to-rows ccfiStr)))
    (let ((height (cl:length rows)) (width nil) (y 0))
      (dolist (row rows)
	(let ((scanned-row (scan-row row)))
	  (let ((cur-row-width (row-width scanned-row)))
	    (if (not width)
		(progn
		  (setf width cur-row-width)
		  (funcall createBoardFn width height))
		(process-parsed-row 
		 (lambda (x y token)
		   (if (>= x width)
		       (error 'invalid-field-definition-error :text "Rows must have same length")
		       (funcall setBoardFieldFn x y token)))
		 y scanned-row))))
      (setf y (+ y 1))
      ))))
