
#|
Interface of the server
|#


(in-package :ccfi)

(defclass server ()
  ((name :initarg :name :initform "OllisServer" :accessor name)))

(defgeneric put-command (server command)
  (:documentation "Add a command to the comamnd quue"))
(defgeneric write-message (server message))
(defgeneric connected (server))
(defgeneric disconnected (server))


