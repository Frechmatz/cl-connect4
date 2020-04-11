
(in-package :connect4-web-server)

(defparameter *server* nil)
(defparameter *port* 7999)
(defparameter *websocket-server* nil)
(defparameter *websocket-port* 7998)

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
      (let ((log-file (merge-pathnames "connect4.log" (user-homedir-pathname))))
	(logger:add-appender (lambda (message)
			       (let ((stream
				      (open log-file
					    :direction :output
					    :if-exists :append
					    :if-does-not-exist :create)))
				 (write-line message stream)
				 (close stream))))))
  (file-error ()))

    


(defun start-main-server ()
  (if *server*
      (format t "~%Web server already running~%")
      (progn
	(setf *server* (hunchentoot:start
			(make-instance
			 'hunchentoot:easy-acceptor
			 :port *port*)))
	(format t "~%Hi there. The web server has been started.")
	(format t "~%The server can be reached via http://localhost:~a" *port*)
	(hunchentoot:define-easy-handler (root :uri "/") ()
	  (logger:log-message :info (format nil "Root page requested. Query parameters: ~a" (hunchentoot:query-string*)))
	  (setf (hunchentoot:content-type*) "text/html")
	  (start-page
	   :dx (param-as-integer (hunchentoot:query-string*) "dx" :default-value 10 :min-value 2 :max-value 20)
	   :dy (param-as-integer (hunchentoot:query-string*) "dy" :default-value 5 :min-value 2 :max-value 20)))
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
	;; (push (defrest:create-rest-table-dispatcher) hunchentoot:*dispatch-table*)
	;; (defrest:defrest "/length/{dx:[0-9]+}/{dy:[0-9]+}" :GET (dx dy)
	;;   (setf (hunchentoot:content-type*) "text/html")
	;;   (start-page :dx dx :dy dy))
	nil
  )))

(defun stop-main-server ()
  (if (not *server*)
      (format t "~%Web server is not running")
      (let ((srv *server*))
	(setf *server* nil)
	(hunchentoot:stop srv)
	(format t "The web server has been stopped.")
	)))

(defun start-websocket-server ()
  (if *websocket-server*
      (format t "~%Websocket server already running~%")
      (progn
	(setf *websocket-server* (connect4-cfi-websocket::start-server :port *websocket-port*)))))

(defun stop-websocket-server ()
  (if (not *websocket-server*)
      (format t "~%Websocket server is not running")
      (let ((srv *websocket-server*))
	(setf *websocket-server* nil)
	(connect4-cfi-websocket::stop-server srv))))

(defun start ()
  (start-main-server)
  (start-websocket-server))

(defun stop ()
  (stop-main-server)
  (stop-websocket-server))

(defun start-page (&key (dx nil) (dy nil))
  (logger:log-message :info (format nil "start-page called with dx=~a, dy=~a" dx dy))
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
			(:ul :class "navbarmenu"
			 (:li :class "navbaritem"
			      (:a :class "link-new-game" :id "link-new-game" :title "New Game" :href "#" (:span "New game")))
			 (:li :class "navbaritem"
			      (:a :class "link-set-board-size" :id "link-set-board-size" :href "#")
			      (:div :class "navbaritemsubdialog set-board-size-form"
				    (:div :class "wrapper"
					  (:div :class "title" (:p "Set board size"))
					  (:form :method "get"
						 (:div :class "input-group"
						       (:label :for "#board-size-form-input-dx" "Width:")
						       (:input :class "input" :type "text" :name "dx" :id "board-size-form-input-dx"))
						 (:div :class "input-group"
						       (:label :for "#board-size-form-input-dy" "Height:")
						       (:input :class "input" :type "text" :name "dy" :id "board-size-form-input-dy"))
						 
						 (:input :type "submit")
						 )
					  ))
			      )
			 ;; (:a :class "link-debug" :id "link-debug" :title "Debug" :href "#" (:span "Debug"))
			 )
			)
		  (:div :class "playground"
			(:div :class "board" :id "board"
			      (cl-who:str
			       (funcall
				#'connect4-board-renderer-experimental:render-ccfi-board
				;; "xxx4/4ooo/7/7/2oooxx/7"
				;;"xxo2ox/oxo4/1xo4/2x4/7/7"
				;;"7/7/7/7/7/7"
				;;"12/12/12/12/12/12/12/12"
				(funcall #'cfi-server:create-placement dx dy)
				))))
		  (:div :class "console"
			(:div :class "console-content"
			      (:textarea :class "console-textarea" :id "console-textarea"))))
	    (:div :class "footer" :id "footer"
		  (:a :class "human-players-token-indicator" :href "#" :title "Toggle Color" (:p "Your Color"))
		  (:a :class "level-indicator" :href "#" :title "Cycle through levels" (:p "Level {level}"))
		  (:a :class "final-state-click-to-continue" :href "#" (:p "Click to continue"))
		  ;; initialize dataset attribute "value" for IE11 compatibility
		  ;; on-load init via JavaScript doesn't work 
		  (:div :class "activity-indicator" :data-value "OFF" (:p "The Computer is thinking."))
		  (:a :class "quit-button" :href "#" :data-value "OFF" (:p "Quit"))
		  )
	    )))))

(defun message ()
  "Connect 4")

