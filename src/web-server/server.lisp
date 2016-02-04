
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
	(format t "~%Enter http://localhost:~a/static/showcase.html for a case study~%" *port*)
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

(defun body-background-color ()
  "linen"
  )

(defun serve-connect4-css ()
  (cl-css:css `(
		(.header
		 :background-color "LightGoldenRodYellow"
		 :background-image "url(/static/made-with-lisp-logo.png)"
		 :background-repeat "no-repeat"
		 :background-size "contain"
		 :height "100px"
		 :background-position "right"
		 ;;:font-size "5vw"
		 :position relative
		 )
		(.board :background-color "green")
		(body :background-color ,(body-background-color))
		(".header h1" 
			 :margin "0"
			 :position "absolute"
			 :top "50%"
			 :-webkit-transform "translate(0, -50%)" ;;; Safari
			 :transform "translate(0, -50%)"
			 )
		(.board
		 :width "70%"
		 :float "left"
		 )
		(.board-table
		 :width "100%"
		 )
		(.console
		 :width "30%"
		 :float "left"
		 )
		(".board-table > td[board-field='X']"
		 :background-color "red")
	      )))

(defun render-board (dx dy)
  (cl-who:with-html-output-to-string (s)
    (:table :class "board-table"
     (:tr
      (:td :board-field "X")
      (:td "X")
      (:td "X"))
     (:tr
      (:td "X")
      (:td "X")
      (:td "X"))
     (:tr
      (:td "X")
      (:td "X")
      (:td "X"))
    )))

;; (render-board 5 6)

(defun start-page ()
  (cl-who:with-html-output-to-string (s)
			 (:html
			  (:head (:title "Connect 4")
				 (:link :rel "stylesheet" :href "connect4.css"))
			  (:body
			   (:div :class "header" (:h1 (cl-who:str (funcall #'message))))
			   (:div :class "board"
				 (cl-who:str (funcall #'render-board 7 6))
				 )
			   ))))

(defun message ()
  "Welcome to Connect 4")

