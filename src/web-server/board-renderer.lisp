
(in-package :connect4-board-renderer)

(defun width-in-percent (dx)
  ;; precision of two digits
  (/ (floor (/ 10000 dx)) 100))

(width-in-percent 7)

(defun render-cell (percent token)
  ;; http://stackoverflow.com/questions/20456694/grid-of-responsive-squares/20457076#20457076
  (let ((style (format nil "float: left; width:~$%; padding-bottom: ~$%; position: relative;" percent percent)))
    (cl-who:with-html-output-to-string (s)
      (:div
       :class "board-cell"
       :style style 
	    (:div
	     :class "board-cell-content"
	     :style "position: absolute"
	     :token token
	     (cl-who:str (format nil "~a" token))
	     )
	    )
      )))

(defun render-row (width get-token-fn)
  (let ((percent (width-in-percent width)))
    (cl-who:with-html-output-to-string (s)
      (:div :class "board-row"
	    (dotimes (col width)
	      (cl-who:str (funcall #'render-cell percent (funcall get-token-fn col))))))))

;; TODO: Impl should forward to render-ccfi-board
(defun render-board (dx dy)
  (cl-who:with-html-output-to-string (s)
    (:div :class "board-table"
	  (dotimes (row dy)
	    (cl-who:str (funcall #'render-row dx (lambda (x) (if (= (rem x 2) 0) "o" "x") )))
	    )
	  )))

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
      (:div :class "board-table"
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

  
(render-ccfi-board "xxx/ooo")
