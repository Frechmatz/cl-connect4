

(in-package :ccfi)

(defclass server ()
  (
   (name :initarg :name :initform "OllisServer" :accessor name)
   ))

(defgeneric add-command (server command)
  (:documentation "Add a command to the server"))

(defgeneric write-message (server message))
(defgeneric quit (server))
(defgeneric is-quitting (server))


