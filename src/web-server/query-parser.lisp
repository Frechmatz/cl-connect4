
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

