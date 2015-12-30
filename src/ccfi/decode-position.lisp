
(in-package :ccfi)

(define-condition invalid-field-definition-error (error)
  ((text :initarg :text :reader text)))

(defun field-token-p (str)
  (or (equal str "o") (equal str "x")))
  
(defun parse-row (str)
  "Parse one row of a ccfi representation of a board"
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
  "Iterate through a parsed row and set tokens of target row"
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
  "One-time helper to calculate the initial board width"
  (let ((w 0))
    (process-parsed-row
     (lambda (x y token)
       (declare (ignore x))
       (declare (ignore y))
       (declare (ignore token))
       (setf w (+ w 1))) 0 scanned-row)
    w))

(defun split-board-to-rows (ccfiStr)
  "Split ccfi board definition into row definitions"
  (if (or (not ccfiStr) (equal ccfiStr ""))
      (error 'invalid-field-definition-error :text (format nil "Board must not be empty"))
      (let ((rows (cl-ppcre:split "/" ccfiStr)))
	(if (not rows)
	    (error 'invalid-field-definition-error :text (format nil "Board must not be empty"))
	    rows))))

(defun decode-position (ccfiStr createBoardFn setBoardFieldFn)
  "Parse a ccfi board definition
ccfiStr The board in ccfi representation
createBoardFn (dx dy): Called once. Set dimensions of board
setBoardFieldFn (x y token): Won't be called with out of range coordinates. Set field value of the board"
  (let ((rows (split-board-to-rows ccfiStr)))
    (let ((height (cl:length rows)) (width nil) (y 0))
      (dolist (row rows)
	(let ((scanned-row (parse-row row)))
	  (if (not width)
	      (progn
		(setf width (row-width scanned-row))
		(funcall createBoardFn width height)))
	  (let ((cur-width nil))
	    (process-parsed-row 
	     (lambda (x y token)
	       (setf cur-width (+ x 1))
	       (if (>= x width)
		   (error 'invalid-field-definition-error :text "Row is too long")
		   (funcall setBoardFieldFn x y token)))
	     y scanned-row)
	    (if (< cur-width width)
		(error 'invalid-field-definition-error :text "Row is too short"))
	    ))
	(setf y (+ y 1))
      ))))
