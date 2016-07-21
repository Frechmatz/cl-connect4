
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

(defgeneric put (cfi-server command)
  (:documentation "Add a command according to the Cfi-Specification to the queue."))

(defgeneric message (cfi-server message)
  (:documentation
   "Abstract callback handler to be implemented by a server.
    Is being called with strings according to the Cfi-Specification"))

(defgeneric start (cfi-server))
(defgeneric stop (cfi-server))

;;
;; Tool functions to be removed from this file
;;

(defun as-comment (str)
  (format nil "# ~a" str))

(defun as-error (str)
  (format nil "# ~a" str))

