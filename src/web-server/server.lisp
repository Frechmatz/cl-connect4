
(in-package :connect4-web-server)

(defparameter *server* nil)
(defparameter *port* 8002)

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*)))

(defun get-static-dir ()
  (let ((basepath
	 (make-pathname :name nil :type nil :version nil :defaults *this-file*)))
    ;; (format t "Basepath is ~a~%" basepath)
    (format nil "~astatic/" basepath) 
    ))


(defun start ()
  (if *server*
      (format t "~%Server already running~%")
      (progn
	(setf *server* (hunchentoot:start
			(make-instance
			 'hunchentoot:easy-acceptor
			 :port *port*)))
	(format t "~%Hi there. The server has been started.")
	(format t "~%The server can be reached via http://localhost:~a" *port*)
	(hunchentoot:define-easy-handler (connect4-css :uri "/connect4.css") ()
	  (setf (hunchentoot:content-type*) "text/css")
	  (connect4-css:css)
	  )
	(hunchentoot:define-easy-handler (connect4-js :uri "/connect4.js") ()
	  (setf (hunchentoot:content-type*) "text/javascript")
	  (connect4-web-server::encode-placement))
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

  
(defun stop ()
  (if (not *server*)
      (format t "~%Server is not running")
      (let ((srv *server*))
	(setf *server* nil)
	(hunchentoot:stop srv)
	(format t "The server has been stopped.")
	)))

(defun start-page ()
  (cl-who:with-html-output-to-string (s)
			 (:html
			  (:head (:title "Connect 4")
				 (:link :rel "stylesheet" :href "connect4.css"))
			  (:body
			   (:div :class "header" (:h1 (cl-who:str (funcall #'message))))
			   (:div :class "board"
				 (cl-who:str (funcall #'connect4-board-renderer:render-board 7 6))
				 )
			   ))))

(defun message ()
  "Welcome to Connect 4")

