
(in-package :connect4-web-server)

(defparameter *server* nil)
(defparameter *port* 8002)

(defun start ()
  (if *server*
      (format t "~%Server already running~%")
      (progn
	(setf *server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port *port*)))
	(format t "~%Hi there. The server has been started.")
	(format t "~%The server can be reached via http://localhost:~a~%" *port*)
	(hunchentoot:define-easy-handler (connect4-js :uri "/connect4.js") (name)
	  (setf (hunchentoot:content-type*) "application/json")
	  (connect4-web-server::encode-placement))
  )))

(defun stop ()
  (if (not *server*)
      (format t "~%Server is not running")
      (let ((srv *server*))
	(setf *server* nil)
	(hunchentoot:stop srv)
	(format t "The server has been stopped.")
	)))



