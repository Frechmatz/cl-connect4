;;
;; Helper functions to format an info response
;;

(in-package :cfi-server)

(defun format-info (info)
  (format nil "info --plies ~a" (second (assoc :plies info))))

