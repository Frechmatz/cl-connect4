
;;
;; Pretty printer for a board
;;

(defclass board-formatter () ())

(defgeneric format-board (formatter board &optional highlight-cells)
  (:documentation "Format a board"))

(defgeneric format-horizontal-cell-margin(formatter)
  (:documentation "Format the horizontal space between two cells in a row"))

(defgeneric format-cell-value (formatter cell-value)
  (:documentation "Formats a single cell represented by its value"))

(defgeneric format-cell-value-highlighted (formatter cell-value)
  (:documentation "Formats a single cell represented by its value as highlighted"))

(defgeneric format-cell (formatter board x y &optional highlight-cell-p)
  (:documentation "Formats a single cell represented by its position within the board"))

(defgeneric format-border-top (formatter x)
  (:documentation "Format the header of given column"))

(defgeneric format-border-bottom (formatter x)
  (:documentation "Format the footer of given column"))

(defgeneric format-border-left (formatter y)
  (:documentation "Format the left border of given row"))

(defgeneric format-border-right (formatter y)
  (:documentation "Format the right border of given row"))

(defmethod format-cell-value ( (formatter board-formatter) cell-value)
  (cond
   ((equal cell-value *WHITE*) "W")
   ((equal cell-value *BLACK*) "B")
   (t "_")
   ))

(defmethod format-cell-value-highlighted ( (formatter board-formatter) cell-value)
  (cond
   ((equal cell-value *WHITE*) "W")
   ((equal cell-value *BLACK*) "B")
   (t "_")
   ))

(defmethod format-horizontal-cell-margin( (formatter board-formatter))
  " ")

(defmethod format-cell ( (formatter board-formatter) board x y &optional highlight-cell-p)
  (if highlight-cell-p
      (format-cell-value-highlighted formatter (get-field board x y))
    (format-cell-value formatter (get-field board x y))
   ))

(defmethod format-border-top ( (formatter board-formatter) x)
  (format nil "~1,'0x" x)
   )
(defmethod format-border-bottom ( (formatter board-formatter) x)
  (format nil "~1,'0x" x)
   )
(defmethod format-border-left ( (formatter board-formatter) y)
  (format nil "~1,'0x" y)
   )
(defmethod format-border-right ( (formatter board-formatter) y)
  (format nil "~1,'0x" y)
   )

;;
;;
;;
(defclass colorful-board-formatter (board-formatter) ())

;; override simple B/W formatting
(defmethod format-cell-value ( (formatter colorful-board-formatter) cell-value)
  (cond
   ((equal cell-value *WHITE*) (format nil "~c[32mW~c[0m" #\Esc #\Esc))
   ((equal cell-value *BLACK*) (format nil "~c[31mB~c[0m" #\Esc #\Esc))
   (t "_")
   ))

(defmethod format-cell-value-highlighted ( (formatter colorful-board-formatter) cell-value)
  (cond
   ((equal cell-value *WHITE*) (format nil "~c[32;1mW~c[0m" #\Esc #\Esc))
   ((equal cell-value *BLACK*) (format nil "~c[31;1mB~c[0m" #\Esc #\Esc))
   (t "_")
   ))


(defun is-highlight-cell (x y &optional highlight-cells)
    (if (not highlight-cells) nil
      (let ((tupel (list x y)))
	(find-if (lambda (p) (equal p tupel)) highlight-cells)
	)))


(defmethod format-board ( (formatter board-formatter) board &optional highlight-cells)
  (let ((inner-height (get-board-height board)) (inner-width (get-board-width board)))
    (let ( (formatted-board (make-array (list (+ 2 inner-height) (+ 2 inner-width)) :initial-element "X")))
      ;; format inner field
      (dotimes (y inner-height)
	(dotimes (x inner-width)
	  (setf (aref formatted-board (+ y 1) (+ x 1)) (format-cell formatter board x y (is-highlight-cell x y highlight-cells)))
	  ))
      ;; format top/bottom border
      (dotimes (x inner-width)
	(setf (aref formatted-board 0 (+ x 1)) (format-border-top formatter x))
	(setf (aref formatted-board (+ inner-height 1) (+ x 1)) (format-border-bottom formatter x))
	)
      ;; format left/right border
      (dotimes (y inner-height)
	(setf (aref formatted-board (+ y 1) 0) (format-border-left formatter y))
	(setf (aref formatted-board (+ y 1) (+ inner-width 1)) (format-border-right formatter y))
	)
      ;; inner function that joins all strings of a given row
      (flet ((join-row (y)
		       (let ((result (aref formatted-board y 0)))
			 (dotimes (x (+ (array-dimension formatted-board 1) -1))
			   (setf result (concatenate 'string result (format-horizontal-cell-margin formatter)))
			   (setf result (concatenate 'string result (aref formatted-board y (+ x 1))))
			   )
			 result)))
	    (dotimes (y (array-dimension formatted-board 0))
	      (princ (join-row y))
	      (princ #\newline))
	    )
      )
    )
  nil
  )

