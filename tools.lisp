
#|
Tool functions
|#


;; Get the maximum value of a list of numbers
;; (max-list-value '(1 2 3)) => 3
(defun max-list-value (list &optional valueFn)
  (if (not valueFn) (setf valueFn (lambda (x) x)))
  (let ((result (car list)))
    (let ((m (funcall valueFn result)) (next-value nil))
      (dolist (elem (cdr list))
	(setf next-value (funcall valueFn elem))
	(if (> next-value m) (progn (setf m next-value) (setf result elem)))
      ))
    result)
  )

;; Get the minimum value of a list of numbers
;; Todo: Combine min and max into one function
(defun min-list-value (list &optional valueFn)
  (if (not valueFn) (setf valueFn (lambda (x) x)))
  (let ((result (car list)))
    (let ((m (funcall valueFn result)) (next-value nil))
      (dolist (elem (cdr list))
	(setf next-value (funcall valueFn elem))
	(if (< next-value m) (progn (setf m next-value) (setf result elem)))
      ))
    result)
  )
