(in-package :connect4-cfi-websocket)

;;
;; Class that implements the message interface of cfi-server
;;

(defclass cl-websocket-connect4-server (cfi-server:cfi-server)
  ((web-socket-client :initform nil)))

(defmethod cfi-server:message ((the-server cl-websocket-connect4-server) message)
  (clws.handler:send-text-message (slot-value the-server 'web-socket-client) message))

;;
;; cl-websocket handler class
;; - Forwards text messages to the CFI-Server
;; - Sends back messages of the CFI-Server to the socket listener (e.g. a browser)
;;

(defclass ccfi-handler (clws.handler:connection-handler)
  ((connect4-server :initform nil)))

(defmethod clws.handler:on-open-connection ((handler ccfi-handler))
  ;; Connection has been opened -> Instantiate core connect4-server
  (let ((c4-server (make-instance 'cl-websocket-connect4-server)))
    (setf (slot-value c4-server 'web-socket-client) handler)
    (setf (slot-value handler 'connect4-server) c4-server))
  (logger:log-message :info "Client connected"))

(defmethod clws.handler:on-close-connection ((handler ccfi-handler) status-code reason)
  (logger:log-message :info "Client disconnected")
  (cfi-server:put (slot-value handler 'connect4-server) "stop"))

(defmethod clws.handler:on-text-message ((handler ccfi-handler) message)
  (logger:log-message :info  (format nil "Text message received: ~a" message))
  (cfi-server:put (slot-value handler 'connect4-server) message))

;;
;; Start/Stop server
;;

(defun start-server (&key port)
  (format t "~%Starting websocket server...")
  (let ((server (clws.server:make-websocketserver "localhost" port)))
    (clws.server:register-resource-handler server "/ccfi" 'ccfi-handler '())
    (clws.server:start server)
    (format t "~%The websocket server has been started.")
    (format t "~%The websocket server can be reached via http://localhost:~a" port)
    server))

(defun stop-server (server)
  (clws.server:stop server)
  (format t "~%The websocket server has been stopped."))
