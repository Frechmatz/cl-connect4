#|
Render a board using a HTML table
|#

(in-package :connect4-board-renderer)

(defun width-in-percent (dx)
  ;; precision of two digits
  (/ (floor (/ 10000 dx)) 100))

;;(width-in-percent 7)

(defun render-cell (percent token)
  ;; If borders are enabled, the resulting cells are not exactly square
  ;; because the borders expand the content area
  (let ((style (format nil "width:~$%; padding-bottom: ~$%;" percent percent)))
    (cl-who:with-html-output-to-string (s)
      (:td
       :class "board-cell"
       :style style
       :token token
      ))))

(defun render-row (width get-token-fn)
  (let ((percent (width-in-percent width)))
    (cl-who:with-html-output-to-string (s)
      (:tr :class "board-row"
	    (dotimes (col width)
	      (cl-who:str (funcall #'render-cell percent (funcall get-token-fn col))))))))

(defun render-ccfi-board (ccfi-placement)
  (let ((board nil) (width nil) (height nil))
    (ccfi::decode-placement
     ccfi-placement
     (lambda (dx dy)
       (setf width dx)
       (setf height dy)
       (setf board (make-array (list dy dx) :initial-element "Q")))
     (lambda (x y token)
       (setf (aref board y x) token)))
    (cl-who:with-html-output-to-string (s)
      (:table :class "board-table"
	    (dotimes (row height)
	      (cl-who:str
	       (funcall
		#'render-row width
		(lambda (x)
		  (let ((field (aref board (- height row 1) x)))
		    (if (not field)
			"_"
			(if (equal field "o") "o" "x")
			))))))))))

  
;;(render-ccfi-board "xxx/ooo")
