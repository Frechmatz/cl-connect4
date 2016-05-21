#|
A simple logger
|#

(in-package :logger)

(defun get-current-time-string ()
  (multiple-value-bind (sec min hour) (get-decoded-time)
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour min sec)))

(defun format-message (level message)
  (format nil "~a ~a: ~a" (get-current-time-string) level message))

(defparameter *appender* (list
			  (lambda (message) (princ message))))

;; Levels: :debug, :warn, :info, :error
(defun log-message (level message)
  (let ((formatted-message (format-message level message)))
    (dolist (appender *appender*)
      (funcall appender formatted-message))))

;; appender(message)
(defun add-appender (appender)
  (push appender *appender*))

