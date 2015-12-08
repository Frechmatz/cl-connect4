
(in-package :connect4-server)

(defparameter *server* nil)

(defun start ()
  (if *server*
      (format t "~%Server already running~%")
      (progn
	(format t "~%Hi there. Server here!~%")
	(setf *server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8002))))
  ))

(defun stop ()
  (if (not *server*)
      (format t "~%Server is not running")
      (let ((srv *server*))
	(setf *server* nil)
	(hunchentoot:stop srv)
	)))



