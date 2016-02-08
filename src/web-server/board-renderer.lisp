
(in-package :connect4-board-renderer)


(defun render-cell (percent)
  ;; http://stackoverflow.com/questions/20456694/grid-of-responsive-squares/20457076#20457076
  (let ((style (format nil "float: left; width:~a%; padding-bottom: ~a%; position: relative;" percent percent)))
    (cl-who:with-html-output-to-string (s)
      (:div
       :class "board-cell"
       :style style 
	    (:div
	     :class "board-cell-content"
	     :style "position: absolute; height: 80%; width: 80%; padding: 10% 10%;" "Z")
	    )
      )))

(defun render-row (width)
  (let ((row "") (percent (floor (/ 100 width))))
    (dotimes (col width)
      (setf row (concatenate 'string row (render-cell percent))))
    row)
  )

(defun render-board (dx dy)
  (cl-who:with-html-output-to-string (s)
    (:div :class "board-table"
	  (dotimes (row dy)
	    (cl-who:str (funcall #'render-row dx))
	    )
	  )))

