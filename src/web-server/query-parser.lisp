
(in-package :connect4-web-server)


(defun param-as-positive-integer (params param-name &key (default-value nil))
  ;; See also http://weitz.de/cl-ppcre/
  ;; "If there is no match, the statement* forms are not executed."
  (let ((result (cl-ppcre:register-groups-bind (value)
		    ((concatenate 'string param-name "=" "([\\d]+)") params)
		  (if value
		      (let ((v (parse-integer value)))
			(if (> v 0)
			    v
			    nil))))))
    (if (not result)
	default-value
	result)))

(defun param-as-integer (params param-name &key (default-value nil) (min-value nil) (max-value nil))
  ;; See also http://weitz.de/cl-ppcre/
  ;; "If there is no match, the statement* forms are not executed."
  (let ((result (cl-ppcre:register-groups-bind (value)
		    ((concatenate 'string param-name "=" "([+-]?[\\d]+)") params)
		  (if value
		      (parse-integer value)
		      nil))))
    (if (and (not result) default-value)
	(setf result default-value))
    (if (and min-value result (< result min-value))
	(setf result min-value))
    (if (and max-value result (> result max-value))
	(setf result max-value))
    result))
