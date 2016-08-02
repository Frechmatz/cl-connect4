
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
   (name :initarg :name :initform "CfiServer" :accessor name)
   (command-queue :initform (queues:make-queue :simple-cqueue))
   (quit-flag :initform nil)
   (server-state :initform +SERVER-STATE-INITIALIZED+)
   (worker-state :initform +WORKER-STATE-WORKER-NOT-STARTED+)
   (server-lock :initform (bt:make-lock "server-lock"))
   ))

(defgeneric put (cfi-server command)
  (:documentation
   "Add a command to the queue. Commands must conform to the Cfi-Specification."))

(defgeneric message (cfi-server message)
  (:documentation
   "Callback handler to be implemented by a server.
    Is being called with response messages according to the Cfi-Specification.
    This function will not be called if the server has stopped 
    or is stopping."))

(defgeneric get-state (cfi-server)
  (:documentation
   "Get the state of the server. Returns an alist holding the 
    properties :server-state and :worker-state"))
  
