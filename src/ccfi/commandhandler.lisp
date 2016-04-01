
(in-package :ccfi)

(defparameter *handler* '
  ((quit . quit-handler)
   (play . play-handler)))


(defun as-keyword (sym)
  (intern (string (string-upcase sym)) :keyword))

(defun preprocess-parameter (parameter)
  (if (and (>= (length parameter) 3) (string= parameter "--" :start1 0 :end1 2 :start2 0 :end2 2))
      (as-keyword (subseq parameter 2))
      parameter))

(defun build-lambda-list (list)
  (mapcar #'preprocess-parameter list))

(define-condition invalid-arguments (error)
  ((text :initarg :text :reader text)))

(defun quit-handler (server)
  )





