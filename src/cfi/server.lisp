
#|
Interface of the server
|#


(in-package :cfi)

(defclass server ()
  ((name :initarg :name :initform "OllisServer" :accessor name)))

(defgeneric put-command (server command)
  (:documentation "Add a command to the comamnd queue"))
(defgeneric write-message (server message))
(defgeneric connected (server))
(defgeneric disconnected (server))


