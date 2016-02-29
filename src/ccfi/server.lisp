
(in-package :ccfi)

(defparameter *logger* (make-instance 'logger:file-logger :name "ccfi-server"))

(defclass server ()
  (
   (name :initarg :name :initform "OllisServer" :accessor name)
   ))

(defgeneric add-command (server command)
  (:documentation "Add a command the server"))

(defgeneric write-message (server message)
  )

(defclass default-server (server)
  (
   (difficulty-level :initarg :difficulty-level :initform 6 :accessor difficulty-level)
   (command-queue :initform '() :accessor command-queue)
   ))

(defmethod add-command ((server default-server) command)
  (logger:log-info *logger* (format nil "default-server add command called: ~a" command))
  ;; Echo command
  (write-message server (format nil "ECHO: ~a" command))
  )

