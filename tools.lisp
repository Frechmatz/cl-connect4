
#|
Tool functions
|#


;; Get the maximum value of a list of numbers
;; (max-list-value '(1 2 3)) => 3
(defun max-list-value (list)
  (let ((m (car list)))
    (dolist (v (cdr list))
      (setf m (max m v))
      )
    m)
  )

