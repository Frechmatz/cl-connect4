
(in-package :logger)

(defclass logger ()
  (
   (name :initarg :name :initform "logger" :accessor name)
   )
  )

(defgeneric log-info (logger str)
  (:documentation "Log information"))

(defgeneric log-warn (logger str)
  (:documentation "Log warning"))

(defgeneric log-error (logger str)
  (:documentation "Log error"))

(defun format-message (logger level str)
  (format nil "~a ~a: ~a" level (slot-value logger 'name) str))

(defclass console-logger (logger) ()
  (:documentation "A console logger")
  )

(defmethod log-info ( (logger console-logger) str)
  (princ (format-message logger "INFO" str))
  )

(defmethod log-warn ( (logger console-logger) str)
  (princ (format-message logger "WARN" str))
  )
(defmethod log-error ( (logger console-logger) str)
  (princ (format-message logger "ERROR" str))
  )

(defclass file-logger (logger)
  ((filename :initarg :filename
	     :initform "/Users/olli/var/log/log.txt"
	     :accessor filename)))

(defun write-to-file (filename str)
  (let ((stream (open filename :direction :output :if-exists :append :if-does-not-exist :create)))
    (write-line str stream)
    (close stream)))

(defmethod log-info ( (logger file-logger) str)
  (write-to-file (slot-value logger 'filename) (format-message logger "INFO" str))
  )

(defmethod log-warn ( (logger file-logger) str)
  (write-to-file (slot-value logger 'filename) (format-message logger "WARN" str))
  )
(defmethod log-error ( (logger file-logger) str)
  (write-to-file (slot-value logger 'filename) (format-message logger "ERROR" str))
  )


(defun test-it ()
  (let ((myLogger (make-instance 'file-logger)))
    (log-info myLogger "Hallo")
    ))

;; (test-it)
