;;
;; Console-Client of the Cfi-Server
;;

(in-package :connect4-cfi-console)

(logger:add-appender (lambda (message)
		       (let ((stream
			      (open "/Users/olli/var/log/log.txt"
				    :direction :output
				    :if-exists :append
				    :if-does-not-exist :create)))
			 (write-line message stream)
			 (close stream))))

(defclass connect4-server (cfi-server:cfi-server)
  (
   (message-lock :initform (bt:make-lock "message-lock") :accessor message-lock)
   ))

(let ((out *standard-output*))
  (defmethod cfi-server:message ((the-server connect4-server) message)
  (bt:with-lock-held ((slot-value the-server 'message-lock))
    (format out "~a~%" message)
    (finish-output out))))

(defun read-cmd ()
  (read-line))

(defun lets-go ()
  (let ((server (make-instance 'connect4-server)))
    (start server)
    (format t "Started server~%")
    (loop
       (format t "Enter command (stop or any cfi-command)~%")
       (let ((cmd (read-cmd)))
	 (cond
	   ((string= cmd "stop")
	    (stop server)
	    (return))
	   (t (put server cmd)
	      ))))))
