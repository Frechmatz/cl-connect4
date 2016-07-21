
;;
;; Declaration of the cfi-server interface
;;

(in-package :cfi-server)

(defconstant +SERVER-STATE-INITIALIZED+ 1)
(defconstant +SERVER-STATE-RUNNING+ 2)
(defconstant +SERVER-STATE-STOPPING+ 3)
(defconstant +SERVER-STATE-STOPPED+ 4)

(defconstant +WORKER-STATE-WORKER-NOT-STARTED+ 0)
(defconstant +WORKER-STATE-IDLE+ 1)
(defconstant +WORKER-STATE-PROCESSING+ 2)
(defconstant +WORKER-STATE-QUITTING+ 3)
(defconstant +WORKER-STATE-TERMINATED+ 4)

(defclass cfi-server ()
  (
   (name :initarg :name :initform "OllisServer" :accessor name)
   (command-queue :initform (queues:make-queue :simple-cqueue))
   (quit-flag :initform nil)
   (server-state :initform +SERVER-STATE-INITIALIZED+)
   (worker-state :initform +WORKER-STATE-WORKER-NOT-STARTED+)
   (server-lock :initform (bt:make-lock "server-lock"))
   ))


(defgeneric start (cfi-server)
  (:documentation
   "Starts the queue processing"))

(defgeneric stop (cfi-server)
  (:documentation
   "Signal that the server shall stop working as soon as possible."))

(defgeneric put (cfi-server command)
  (:documentation
   "Add a command to the queue. Commands must apply to the Cfi-Specification. 
   If the server hasn't been started or has been stopped, the command is ignored."))

(defgeneric message (cfi-server message)
  (:documentation
   "Abstract callback handler to be implemented by a server.
    Is being called with strings according to the Cfi-Specification"))

(defgeneric get-state (cfi-server)
  (:documentation
   "Get the state of the server. Returns an alist holding the properties :server-state and :worker-state")
  )
  

;;
;; Tool functions to be removed from this file
;;

(defun as-comment (str)
  (format nil "# ~a" str))

(defun as-error (str)
  (format nil "# ~a" str))

