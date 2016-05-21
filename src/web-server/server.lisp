
(in-package :connect4-web-server)

(defparameter *server* nil)
(defparameter *port* 8002)
(defparameter *websocket-server* nil)
(defparameter *websocket-port* 8003)

(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*)))

(defun get-static-dir (directory-name)
  (let ((basepath
	 (make-pathname :name nil :type nil :version nil :defaults *this-file*)))
    ;; (format t "Basepath is ~a~%" basepath)
    (format nil "~a~a/" basepath directory-name) 
    ))

;; Add log appender pointing to log-file inside my log firectory
(handler-case
    (progn
      (ensure-directories-exist "/Users/olli/var/log")
      (logger:add-appender (lambda (message)
			     (let ((stream
				    (open "/Users/olli/var/log/log.txt"
					  :direction :output
					  :if-exists :append
					  :if-does-not-exist :create)))
			       (write-line message stream)
			       (close stream)))))
    (file-error ()))

    


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
	(hunchentoot:define-easy-handler (root :uri "/") ()
	  (setf (hunchentoot:content-type*) "text/html")
	  (start-page))
	(hunchentoot:define-easy-handler (buttons-debug :uri "/buttons/debug.svg") ()
	  (setf (hunchentoot:content-type*) "image/svg+xml")
	  (connect4-buttons:get-debug-button))
	(hunchentoot:define-easy-handler (buttons-newgame :uri "/buttons/newgame.svg") ()
	  (setf (hunchentoot:content-type*) "image/svg+xml")
	  (connect4-buttons:get-start-new-game-button))
	(hunchentoot:define-easy-handler (buttons-togglecolor :uri "/buttons/togglecolor.svg") ()
	  (setf (hunchentoot:content-type*) "image/svg+xml")
	  (connect4-buttons:get-toggle-color-button))
	(push (hunchentoot:create-folder-dispatcher-and-handler
	       "/static/" ;; Must begin and end with slash
	       (get-static-dir "static"))
	      hunchentoot:*DISPATCH-TABLE*) 
	(push (hunchentoot:create-folder-dispatcher-and-handler
	       "/script/" ;; Must begin and end with slash
	       (get-static-dir "script"))
	      hunchentoot:*DISPATCH-TABLE*) 
	(push (hunchentoot:create-folder-dispatcher-and-handler
	       "/css/" ;; Must begin and end with slash
	       (get-static-dir "css"))
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
  (start-websocket-server))

  
(defun stop ()
  (stop-main-server)
  (stop-websocket-server))

(defun start-page ()
  (cl-who:with-html-output-to-string (s)
    (:html
     (:head (:title "Connect 4")
	    (:link :rel "stylesheet" :href "css/connect4.css"))
     (:body
      (:script :src "script/underscore.js")
      (:script :src "script/async.js")
      (:script :src "script/parser.js")
      (:script :src "script/bestmove.js")
      (:script :src "script/board.js")
      (:script :src "script/gameconsole.js")
      (:script :src "script/consolelistener.js")
      (:script :src "script/bestmovelistener.js")
      (:script :src "script/cficlient.js")
      (:script :src "script/layoutcontroller.js")
      (:script :src "script/gamecontroller.js")
      (:script :src "script/footer.js")
      (:script :src "script/onload.js")
      (:div :class "page-wrapper"
	    (:div :class "header" (:h1 (cl-who:str (funcall #'message))))
	    (:div :class "body" 
		  (:div :class "navbar"
			(:a :class "link-new-game" :id "link-new-game" :title "New Game" :href "#" (:span "New game"))
			;; (:a :class "link-toggle-color" :id "link-toggle-color" :title "Toggle color" :href "#" (:span "Toggle color"))
			;; (:a :class "link-debug" :id "link-debug" :title "Debug" :href "#" (:span "Debug"))
			)
		  (:div :class "playground"
			(:div :class "board" :id "board"
			      (cl-who:str
			       (funcall
				#'connect4-board-renderer-experimental:render-ccfi-board
				;; "xxx4/4ooo/7/7/2oooxx/7"
				;;"xxo2ox/oxo4/1xo4/2x4/7/7"
				;;"7/7/7/7/7/7"
				"12/12/12/12/12/12/12/12"
				))))
		  (:div :class "console"
			(:div :class "console-content"
			      (:textarea :class "console-textarea" :id "console-textarea"))))
	    (:div :class "footer" :id "footer"
		  (:a :class "human-players-token-indicator" :href "#" :title "Toggle Color" (:p "Your Color"))
		  (:a :class "final-state-click-to-continue" :href "#" (:p "Click to continue"))
		  )
	    )))))

(defun message ()
  "Connect 4")

