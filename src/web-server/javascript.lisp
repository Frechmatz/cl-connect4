

(in-package :connect4-javascript)

(defun encode-placement ()
  "Generates the JavaScript function 'encodePlacement( dx, dy, getTokenFn)' function that returns a CCFI board placement representation"
  (parenscript:ps
    (defun encode-placement (dx dy get-token-fn)
      (flet ((scan-row (y dx get-token-fn)
	       (let ((row "") (nil-count 0))
		 (dotimes (x dx)
		   (let ((field (funcall get-token-fn x y)))
		     (if (not field)
			 (setf nil-count (+ nil-count 1))
			 (progn
			   (if (> nil-count 0)
			       (progn
				 (setf row (+ row nil-count))
				 (setf nil-count 0)))
			   (setf row (+ row field)))
			 )))
		 (if (> nil-count 0)
		     (setf row (+ row nil-count)))
		 row
		 ))))
      (let ((result ""))
	(dotimes (y dy)
	  (let ((cur-row (funcall scan-row (- dy y 1) dx get-token-fn)))
	    (if (> (slot-value result 'length) 0)
		(setf result (+ result "/")))
	    (setf result (+ result cur-row))
	    ))
	result
	))))

(defun javascript()
  (encode-placement))
