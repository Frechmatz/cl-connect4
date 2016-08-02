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
  ((message-lock :initform (bt:make-lock "message-lock") :accessor message-lock)))

(let ((out *standard-output*))
  (defmethod cfi-server:message ((the-server connect4-server) message)
  (bt:with-lock-held ((slot-value the-server 'message-lock))
    (format out "~a~%" message)
    (finish-output out))))

(defun stop-server (server)
  "Stop the server and wait until the server has stopped."
  (put server "stop")
  (loop
     (format t  "Stopping server...~%")
     (let ((state (get-state server)))
       (if (eql (second (assoc :server-state state)) cfi-server:+SERVER-STATE-STOPPED+)
	   (progn
	     (format t "Server stopped~%")
	     (return))
	   (sleep 1)))))
	     
(defun read-cmd ()
  (read-line))

(defun lets-go ()
  (let ((server (make-instance 'connect4-server)))
    (format t "Instantiated server~%")
    (loop
       (format t "Enter Cfi command~%")
       (let ((cmd (read-cmd)))
	 (cond
	   ((string= cmd "stop")
	    ;; Stop server and wait until it has stopped
	    (stop-server server)
	    (return))
	   (t (put server cmd)
	      ))))))
