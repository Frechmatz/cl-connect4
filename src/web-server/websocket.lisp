#|
CCFI-Server as a websocket
|#

(in-package :connect4-ccfi-websocket)

(defparameter *logger* (make-instance 'logger:file-logger :name "websocket"))


(defclass ccfi-resource (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this room!") :reader name))
  (:default-initargs :client-class 'ccfi-client))

(defclass ccfi-client (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this ccfi-client!"))))


(defvar *the-ccfi-resource*
   (make-instance 'ccfi-resource :name "/ccfi"))

(defun find-ccfi-resource (request)
  *the-ccfi-resource*)

(pushnew 'find-ccfi-resource hunchensocket:*websocket-dispatch-table*)

(defun broadcast (cur-ccfi-resource message &rest args)
  (logger:log-info *logger* (format nil "Sending message: ~a" message))
  (loop for peer in (hunchensocket:clients cur-ccfi-resource)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defun answer (cur-ccfi-resource message &rest args)
  (logger:log-info *logger* (format nil "Sending message: ~a" message))
  ;; doesn't work
  ;;(hunchensocket:send-text-message cur-ccfi-resource (apply #'format nil message args))
  )

(defmethod hunchensocket:client-connected ((cur-ccfi-resource ccfi-resource) ccfi-client)
  (logger:log-info *logger* "Client connected")
;;  (broadcast cur-ccfi-resource "Connected to ccfi server " (name ccfi-client) (name cur-ccfi-resource)))
  )

(defmethod hunchensocket:client-disconnected ((cur-ccfi-resource ccfi-resource) ccfi-client)
  (logger:log-info *logger* "Client disconnected")
  (broadcast cur-ccfi-resource "Disconnected from ccfi server" (name ccfi-client) (name cur-ccfi-resource)))

(defmethod hunchensocket:text-message-received ((cur-ccfi-resource ccfi-resource) ccfi-client message)
  (logger:log-info *logger* (format nil "Text message received: ~a" message))
  (broadcast cur-ccfi-resource "ccfiok" (name ccfi-client) message)
  ;; (answer cur-ccfi-resource "ccfiokanswer" (name ccfi-client) message)
  )






