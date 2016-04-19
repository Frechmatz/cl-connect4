#|
Render a board using a HTML table
|#

(in-package :connect4-board-renderer)

(defun width-in-percent (dx)
  ;; precision of two digits
  (/ (floor (/ 10000 dx)) 100))

;;(width-in-percent 7)

(defun render-cell (x y percent token)
  ;; If borders are enabled, the resulting cells are not exactly square
  ;; because the borders expand the content area
  (let ((style (format nil "width:~$%; padding-bottom: ~$%; position: relative" percent percent)))
    (cl-who:with-html-output-to-string (s)
      (:td
       :class "board-cell"
       :style style
       :data-token token
       :data-column x
       :data-row y
       :data-human-players-token "?"
       (:div :class "board-cell-marker"
	     ;; display to be set via JavaScript
	     :style "position: absolute; top: 40%; left: 40%; width: 20%; height: 20%; display: none"
	     )
      ))))

(defun render-row (row-number width get-token-fn)
  (let ((percent (width-in-percent width)))
    (cl-who:with-html-output-to-string (s)
      (:tr :class "board-row" :data-row row-number
	    (dotimes (col width)
	      (cl-who:str (funcall #'render-cell col row-number percent (funcall get-token-fn col))))))))

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
      (:table :class "board-table" :id "board-table" :data-width width :data-height height
	    (dotimes (row height)
	      (cl-who:str
	       (funcall
		#'render-row row width
		(lambda (x)
		  (let ((field (aref board (- height row 1) x)))
		    (if (not field)
			"_"
			(if (equal field "o") "o" "x")
			))))))))))

  
;;(render-ccfi-board "xxx/ooo")
