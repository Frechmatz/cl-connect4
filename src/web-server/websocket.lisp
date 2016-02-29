#|
CCFI-Server as a websocket
|#

(in-package :connect4-ccfi-websocket)

(defparameter *logger* (make-instance 'logger:file-logger :name "websocket"))

(defclass connect4-server (ccfi:default-server)
  (
   (websocket-client :initform "Olli" :accessor websocket-client :initarg :websocket-client)
   ))


(defclass ccfi-resource (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this resource") :reader name))
  (:default-initargs :client-class 'ccfi-client))

(defclass ccfi-client (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this ccfi-client!"))
   (ccfi-server :initform nil :accessor ccfi-server)
   ))

(defvar *the-ccfi-resource*
   (make-instance 'ccfi-resource :name "/ccfi"))

(defun find-ccfi-resource (request)
  *the-ccfi-resource*)

(pushnew 'find-ccfi-resource hunchensocket:*websocket-dispatch-table*)

(defun broadcast (cur-ccfi-resource message &rest args)
  (logger:log-info *logger* (format nil "Sending message: ~a" message))
  (loop for peer in (hunchensocket:clients cur-ccfi-resource)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defun answer (cur-ccfi-client message &rest args)
  (logger:log-info *logger* (format nil "Sending message: ~a" message))
  (hunchensocket:send-text-message cur-ccfi-client (apply #'format nil message args))
  )

;; TODO: Study, what this method definition means
(defmethod ccfi::write-message ((the-server connect4-server) message)
  (logger:log-info *logger* (format nil "websocket::ccfi::write-message called: ~a" message))
  (answer (slot-value the-server 'websocket-client) message)
  )

(defmethod hunchensocket:client-connected ((cur-ccfi-resource ccfi-resource) ccfi-client)
  (logger:log-info *logger* "Client connected")
  (setf (slot-value ccfi-client 'ccfi-server) (make-instance 'connect4-server :websocket-client ccfi-client))
  (ccfi:add-command (slot-value ccfi-client 'ccfi-server) "connected")
  )

(defmethod hunchensocket:client-disconnected ((cur-ccfi-resource ccfi-resource) ccfi-client)
  (logger:log-info *logger* "Client disconnected")
  ;;(broadcast cur-ccfi-resource "Disconnected from ccfi server" (name ccfi-client) (name cur-ccfi-resource))
  )

(defmethod hunchensocket:text-message-received ((cur-ccfi-resource ccfi-resource) ccfi-client message)
  (logger:log-info *logger* (format nil "Text message received: ~a" message))
  ;;(broadcast cur-ccfi-resource "ccfiok" (name ccfi-client) message)
  ;; (answer ccfi-client "websocketok" (name ccfi-client) message)
  (logger:log-info *logger* (format nil "Calling ccfi-server add-command"))
  (ccfi:add-command (slot-value ccfi-client 'ccfi-server) message)
  )






