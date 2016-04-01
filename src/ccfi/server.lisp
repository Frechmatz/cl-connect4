
(in-package :ccfi)

(defparameter *logger* (make-instance 'logger:file-logger :name "ccfi-server"))

(defclass default-server (server)
  (
   (difficulty-level :initarg :difficulty-level :initform 6 :accessor difficulty-level)
   (command-queue :initform '() :accessor command-queue)
   (quit-flag :initform nil :accessor quit-flag)
   ))


(defmethod quit ((server default-server))
  (setf (slot-value server 'quit-flag) t))

(defmethod is-quitting ((server default-server))
  (slot-value server 'quit-flag))


(defun format-minmax-result (result)
  (format nil "~a" (first result)))

(defun process-queue (server command-queue)
  (let ((board nil) (player nil))
    (dolist (command command-queue)
      (let ((items (cl-ppcre:split " " command)))
	(if items
	    (if (equal (first items) "position")
		(progn
		  (setf board (ccfi-placement-to-board (second items)))
		  (setf player (ccfi-token-to-color (third items)))
		  (write-message server
				 (format nil "bestmove ~a"
					 (format-minmax-result (connect4-api:minmax board player 6))))
		  )))))))

(defun process-commands (server)
  (logger:log-info *logger* (format nil "process-commands"))
  (let ((q (reverse (slot-value server 'command-queue))))
    (setf (slot-value server 'command-queue) '())
    (process-queue server q)
  ))

(defmethod add-command ((server default-server) command)
  (logger:log-info *logger* (format nil "add-command: ~a" command))
  (if (equal command "go")
      (process-commands server)
      (push command (slot-value server 'command-queue))
      ))







