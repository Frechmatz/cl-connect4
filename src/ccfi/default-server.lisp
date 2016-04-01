
(in-package :ccfi)

(defparameter *logger* (make-instance 'logger:file-logger :name "ccfi-server"))

(defclass default-server (server)
  (
   (command-queue :initform '() :accessor command-queue)
   (quit-flag :initform nil :accessor quit-flag)
   (busy-flag :initform nil :accessor busy-flag)
   ))


(defmethod quit ((server default-server))
  (setf (slot-value server 'quit-flag) t))

(defmethod is-quitting ((server default-server))
  (slot-value server 'quit-flag))

(defun is-busy (server)
  (slot-value server 'busy-flag))

(defun clear-busy (server)
  (setf (slot-value server 'busy-flag) nil))

(defun set-busy (server)
  (setf (slot-value server 'busy-flag) t))

(defun format-minmax-result (result)
  (format nil "~a" (first result)))

(defun command-done (server)
  (write-message server "# Executed a command")
  )

(defun invoke-command (server command)
  (invoke-command-impl server command)
  (command-done server))

(defun invoke-command-impl (server command)
  (set-busy server)
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

	       


(defun process-queue (server command-queue)
  (dolist (command command-queue)
    (invoke-command server command)))

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







