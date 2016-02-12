#|
CCFI-Server as a websocket
|#

(in-package :connect4-ccfi-websocket)


(defclass ccfi-resource (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this room!") :reader name))
  (:default-initargs :client-class 'ccfi-client))

(defclass ccfi-client (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this ccfi-client!"))))


(defvar *ccfi-resources*
  (list
   (make-instance 'ccfi-resource :name "/ccfi")))

(defun find-ccfi-resource (request)
  (first *ccfi-resources*))

(pushnew 'find-ccfi-resource hunchensocket:*websocket-dispatch-table*)

(defun broadcast (cur-ccfi-resource message &rest args)
  (loop for peer in (hunchensocket:clients cur-ccfi-resource)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((cur-ccfi-resource ccfi-resource) ccfi-client)
  (broadcast cur-ccfi-resource "~a has joined ~a" (name ccfi-client) (name cur-ccfi-resource)))

(defmethod hunchensocket:client-disconnected ((cur-ccfi-resource ccfi-resource) ccfi-client)
  (broadcast cur-ccfi-resource "~a has left ~a" (name ccfi-client) (name cur-ccfi-resource)))

(defmethod hunchensocket:text-message-received ((cur-ccfi-resource ccfi-resource) ccfi-client message)
  (broadcast cur-ccfi-resource "~a says ~a" (name ccfi-client) message))





