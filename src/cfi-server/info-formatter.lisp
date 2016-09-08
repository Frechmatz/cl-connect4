;;
;; Helper functions to format an info response
;;

(in-package :cfi-server)


(defparameter *info-formatters* '(format-plies format-final-scores))

(defun format-plies (info)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((plies (second (assoc :plies info))))
    (if plies
	(format nil "--plies ~a" plies)
	"")))

(defun format-final-scores (info)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (list-to-string
   (second (assoc :final-scores info))
   (lambda (i) (format nil "~a:~a" (first i) (format-score (second i))))
   :string-prefix "--scores "
   :item-separator "/"))

(defun format-info (info)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (list-to-string
   *info-formatters*
   (lambda (f) (funcall f info))
   :string-prefix "info "
   :item-separator " "))
