;;
;; Helper functions to create Cfi placement representions
;;

(in-package :cfi-server)

(defun create-placement (dx dy)
  "Create an empty board" 
  (let ((b "") (first t))
    (dotimes (y dy)
      (if (not first)
	  (setf b (concatenate 'string b "/")))
      (setf first nil)
      (setf b (concatenate 'string b (format nil "~a" dx))))
    b))


