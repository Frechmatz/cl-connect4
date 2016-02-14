
(in-package :connect4-web-server)

(defparameter *server* nil)
(defparameter *port* 8002)
(defparameter *websocket-server* nil)
(defparameter *websocket-port* 8003)

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*)))

(defun get-static-dir ()
  (let ((basepath
	 (make-pathname :name nil :type nil :version nil :defaults *this-file*)))
    ;; (format t "Basepath is ~a~%" basepath)
    (format nil "~astatic/" basepath) 
    ))

(defun start-main-server ()
  (if *server*
      (format t "~%Main server already running~%")
      (progn
	(setf *server* (hunchentoot:start
			(make-instance
			 'hunchentoot:easy-acceptor
			 :port *port*)))
	(format t "~%Hi there. The main server has been started.")
	(format t "~%The server can be reached via http://localhost:~a" *port*)
	(hunchentoot:define-easy-handler (connect4-css :uri "/connect4.css") ()
	  (setf (hunchentoot:content-type*) "text/css")
	  (connect4-css:css))
	(hunchentoot:define-easy-handler (connect4-js :uri "/connect4.js") ()
	  (setf (hunchentoot:content-type*) "text/javascript")
	  (connect4-javascript:javascript))
	(hunchentoot:define-easy-handler (root :uri "/") ()
	  (setf (hunchentoot:content-type*) "text/html")
	  (start-page))
	(hunchentoot:define-easy-handler (root-path :uri "/root-path") ()
	  (setf (hunchentoot:content-type*) "text/plain")
	  (concatenate
	   'string
	   (format nil "This file: ~a~%" *this-file*)
	   (format nil "Directory of this file: ~a~%" (make-pathname :name nil :type nil :version nil :defaults *this-file*))
	   (format nil "Folder for static content, e.g. /static/splash.png: ~a~%" (get-static-dir))
	   ))
	(push (hunchentoot:create-folder-dispatcher-and-handler
	       "/static/" ;; Must begin and end with slash
	       (get-static-dir))
	      hunchentoot:*DISPATCH-TABLE*) 
	nil
  )))

(defun stop-main-server ()
  (if (not *server*)
      (format t "~%Main server is not running")
      (let ((srv *server*))
	(setf *server* nil)
	(hunchentoot:stop srv)
	(format t "The main server has been stopped.")
	)))

(defun start-websocket-server ()
  (if *websocket-server*
      (format t "~%Websocket server already running~%")
      (progn
	(setf *websocket-server* (make-instance 'hunchensocket:websocket-acceptor :port *websocket-port*))
	(hunchentoot:start *websocket-server*)
	(format t "~%The websocket server has been started.")
	(format t "~%The websocket server can be reached via http://localhost:~a" *websocket-port*)
	)
  ))

(defun stop-websocket-server ()
  (if (not *websocket-server*)
      (format t "~%Websocket server is not running")
      (let ((srv *websocket-server*))
	(setf *websocket-server* nil)
	(hunchentoot:stop srv)
	(format t "~%The websocket server has been stopped.")
	)))


(defun start ()
  (start-main-server)
  (start-websocket-server)
  )

  
(defun stop ()
  (stop-main-server)
  (stop-websocket-server)
  )

(defun start-page ()
  (cl-who:with-html-output-to-string (s)
			 (:html
			  (:head (:title "Connect 4")
				 (:link :rel "stylesheet" :href "connect4.css"))
			  (:body
			   ;; hard coded onload.js to be replaced with generator function
			   (:script :src "static/onload.js")
			   (:div :class "page-wrapper"
				 (:div :class "header" (:h1 (cl-who:str (funcall #'message))))
				 (:div :class "body" 
				       (:div :class "board"
					     (cl-who:str (funcall
							  #'connect4-board-renderer:render-ccfi-board
							  "xxx4/4ooo/7/7/2oooxx/7")))
				       (:div :class "console"
					     (:div :class "console-content"
						   (:textarea :class "console-textarea" :id "console-textarea")))
				       ;; "Close" body (parts may be positioned floating) 
				       (:div :style "clear: both")
				       )
				 (:div :class "footer" "Footer")
				 )))))

(defun message ()
  "Connect 4")

