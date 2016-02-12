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
  (find (hunchentoot:script-name request) *ccfi-resources* :test #'string= :key #'name))

(pushnew 'find-ccfi-resource hunchensocket:*websocket-dispatch-table*)


(defun broadcast (room message &rest args)
  (loop for peer in (hunchensocket:clients room)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

(defmethod hunchensocket:client-connected ((room ccfi-resource) ccfi-client)
  (broadcast room "~a has joined ~a" (name ccfi-client) (name room)))

(defmethod hunchensocket:client-disconnected ((room ccfi-resource) ccfi-client)
  (broadcast room "~a has left ~a" (name ccfi-client) (name room)))

(defmethod hunchensocket:text-message-received ((room ccfi-resource) ccfi-client message)
  (broadcast room "~a says ~a" (name ccfi-client) message))





