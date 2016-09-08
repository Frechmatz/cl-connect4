
(in-package :cfi-server)

(defun list-to-string (list itemMapFn &key (item-separator " ") (string-prefix ""))
  (if list
      (concatenate 'string string-prefix
		   (reduce
		    (lambda (a b)
		      (concatenate 'string a (format nil "~a~a" (if (> (length a) 0) item-separator "") b)))
		    (mapcar itemMapFn list)
		    :initial-value ""))
      ""))

(defun format-score (s)
  (format nil "~$" s))
