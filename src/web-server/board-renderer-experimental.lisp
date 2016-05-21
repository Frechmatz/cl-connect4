#|
Experimental board renderer using divs to layout the play field
|#

(in-package :connect4-board-renderer-experimental)

(defun to-percent (d)
  ;; precision of two digits
  ;; add 1 to d in order to allocate some space for the borders
  (/ (floor (/ 10000 (+ 1 d))) 100))

(defun cell-size (board-width board-height)
  (list 
   (to-percent board-width)
   (to-percent board-height)))

(defun render-cell (x y cell-size token)
  (let ((style (format nil "float: left; width:~$%; height: ~$%; position: relative; border: 1px solid black;" (first cell-size) (second cell-size))))
    (cl-who:with-html-output-to-string (s)
      (:div
       :class "board-cell"
       :style style
       :data-token token
       :data-column x
       :data-row y
       :data-human-players-token "?"
       (:div :class "board-cell-marker"
	     ;; display property to be set via CSS
	     :style "position: absolute; top: 40%; left: 40%; width: 20%; height: 20%;"
	     :data-value "OFF"
	     )))))

(defun render-row (row-number width height get-token-fn)
  (let ((cell-size (cell-size width height)))
    (cl-who:with-html-output-to-string (s)
      (:div :class "board-row" :data-row row-number :style "clear:both"
	    (dotimes (col width)
	      (cl-who:str (funcall #'render-cell col row-number cell-size (funcall get-token-fn col))))))))

(defun render-ccfi-board (ccfi-placement)
  (let ((board nil) (width nil) (height nil))
    (cfi::decode-placement
     ccfi-placement
     (lambda (dx dy)
       (setf width dx)
       (setf height dy)
       (setf board (make-array (list dy dx) :initial-element "Q")))
     (lambda (x y token)
       (setf (aref board y x) token)))
    (cl-who:with-html-output-to-string (s)
      (:div :class "board-table" :id "board-table" :data-width width :data-height height
	    (dotimes (row height)
	      (cl-who:str
	       (funcall
		#'render-row row width height
		(lambda (x)
		  (let ((field (aref board (- height row 1) x)))
		    (if (not field)
			"_"
			(if (equal field "o") "o" "x")
			))))))))))


