
(in-package :cfi-server)



(defun create-placement (dx dy)
  (let ((b "") (first t))
    (dotimes (y dy)
      (if (not first)
	  (setf b (concatenate 'string b "/")))
      (setf first nil)
      (setf b (concatenate 'string b (format nil "~a" dx))))
    b))


