#|
CFI-Server as a websocket
|#

(in-package :connect4-cfi-websocket)

(defclass connect4-server (cfi:cfi-server)
  ((websocket-client :initform "Olli" :accessor websocket-client :initarg :websocket-client)))

(defclass cfi-resource (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this resource") :reader name))
  (:default-initargs :client-class 'cfi-client))

(defclass cfi-client (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this cfi-client!"))
   (cfi-server :initform nil :accessor cfi-server)))

(defvar *the-cfi-resource*
   (make-instance 'cfi-resource :name "/ccfi"))

(defun find-cfi-resource (request)
  *the-cfi-resource*)

(pushnew 'find-cfi-resource hunchensocket:*websocket-dispatch-table*)

(defun answer (cur-cfi-client message &rest args)
  (logger:log-message :info (format nil "Sending message: ~a" message))
  (hunchensocket:send-text-message cur-cfi-client (apply #'format nil message args)))

(defmethod cfi::write-message ((the-server connect4-server) message)
  (answer (slot-value the-server 'websocket-client) message))

(defmethod hunchensocket:client-connected ((cur-cfi-resource cfi-resource) cfi-client)
  (logger:log-message :info  "Client connected")
  (setf (slot-value cfi-client 'cfi-server) (make-instance 'connect4-server :websocket-client cfi-client))
  (cfi:start (slot-value cfi-client 'cfi-server)))

(defmethod hunchensocket:client-disconnected ((cur-cfi-resource cfi-resource) cfi-client)
  (logger:log-message :info "Client disconnected")
  (cfi:stop (slot-value cfi-client 'cfi-server)))

(defmethod hunchensocket:text-message-received ((cur-cfi-resource cfi-resource) cfi-client message)
  (logger:log-message :info  (format nil "Text message received: ~a" message))
  (cfi:put-command (slot-value cfi-client 'cfi-server) message))

