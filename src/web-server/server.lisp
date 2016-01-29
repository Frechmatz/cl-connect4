
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
	(hunchentoot:define-easy-handler (connect4-css :uri "/connect4.css") ()
	  (setf (hunchentoot:content-type*) "text/css")
	  (serve-connect4-css)
	  )
	(hunchentoot:define-easy-handler (connect4-js :uri "/connect4.js") ()
	  (setf (hunchentoot:content-type*) "text/javascript")
	  (connect4-web-server::encode-placement))
	(hunchentoot:define-easy-handler (root :uri "/") ()
	  (setf (hunchentoot:content-type*) "text/html")
	  (start-page))
	nil
  )))

(defun stop ()
  (if (not *server*)
      (format t "~%Server is not running")
      (let ((srv *server*))
	(setf *server* nil)
	(hunchentoot:stop srv)
	(format t "The server has been stopped.")
	)))

(defun serve-connect4-css ()
  (cl-css:css '(
		(.header :background-color "yellow")
		(.board :background-color "green")
	      )))

(defun start-page ()
  (cl-who:with-html-output-to-string (s)
			 (:html
			  (:head (:title "Connect 4")
				 (:link :rel "stylesheet" :href "connect4.css"))
			  (:body
			   (:div :class "header" (cl-who:str (funcall #'message)))
			   (:div :class "board" "board")
			   ))))

(defun message ()
  "Welcome to Connect 4!")

