
(in-package :ccfi)

(defparameter *logger* (make-instance 'logger:file-logger :name "ccfi-server"))

(defclass default-server (server)
  (
   (command-queue :initform (make-instance 'queue) :accessor command-queue)
   (quit-flag :initform nil :accessor quit-flag)
   (current-command :initform nil :accessor current-command)
   ))


(defmethod quit ((server default-server))
  (setf (slot-value server 'quit-flag) t))

(defmethod is-quitting ((server default-server))
  (slot-value server 'quit-flag))

(defun is-current-command (server)
  (slot-value server 'current-command))

(defun set-current-command (server cmd)
  (setf (slot-value server 'current-command) cmd))

(defun format-minmax-result (result)
  (format nil "~a" (first result)))

(defun continue-processing (server)
  (if (is-current-command server)
      nil
      (let ((cmd (next (slot-value server 'command-queue))))
	(if cmd
	    (invoke-command server cmd)
	    (write-message server (format nil "ready"))
	    ))))

(defun push-command (server command)
  (put (slot-value server 'command-queue) command))
   
(defun invoke-command (server command)
  (write-message server (format nil "# invoke-command ~a" command))
  (set-current-command server command)
  (invoke-command-impl server command)
  (set-current-command server nil)
  (continue-processing server))

(defun invoke-command-impl (server command)
  (let ((board nil) (player nil))
    (let ((items (cl-ppcre:split " " command)))
      (if items
	  (if (equal (first items) "position")
	      (progn
		(setf board (ccfi-placement-to-board (second items)))
		(setf player (ccfi-token-to-color (third items)))
		(write-message server
			       (format nil "bestmove ~a"
				       (format-minmax-result (connect4-api:minmax board player 6))))
		))))))


(defmethod add-command ((server default-server) command)
  (logger:log-info *logger* (format nil "add-command: ~a" command))
  (push-command server command)
  (continue-processing server))


