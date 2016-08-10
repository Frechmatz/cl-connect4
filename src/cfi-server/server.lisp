;;
;; Implementation of the server-class methods
;;

(in-package :cfi-server)

(defun as-debug (str)
  (format nil "debug ~a" str))

;; message 'wrapper' that ensures that the message function won't be
;; called if the server has been stopped. Assumes that a lock is
;; being held
(defun save-send-message-no-lock (server msg)
  (if (or
       (eql +SERVER-STATE-INITIALIZED+ (slot-value server 'server-state))
       (eql +SERVER-STATE-RUNNING+ (slot-value server 'server-state)))
      (progn
	(logger:log-message :debug (format nil "Sending ~a" msg))
	(message server msg))
      (logger:log-message :debug (format nil "Could not send ~a" msg))))
  
(defun save-send-message-with-lock (server msg)
  (bt:with-lock-held ((slot-value server 'server-lock))
    (save-send-message-no-lock server msg)))

(defmethod get-state ((server cfi-server))
  (bt:with-lock-held ((slot-value server 'server-lock))
  (list
   (list :server-state (slot-value server 'server-state))
   (list :worker-state (slot-value server 'worker-state)))))


(defun start (server)
  "Starts the server. Does not aquire a lock"
    (cond
      ((not (eql +SERVER-STATE-INITIALIZED+ (slot-value server 'server-state)))
       (save-send-message-no-lock server (as-debug "The server cannot be re-started")))
      (t
       (setf (slot-value server 'server-state) +SERVER-STATE-RUNNING+)
       (save-send-message-no-lock server "started")
       (bt:make-thread (lambda ()
			 (logger:log-message :debug "Worker thread: Started")
			 (loop
			    (let ((is-stop-server nil) (next-command nil) (is-quit-commands nil))
			      (bt:with-lock-held ((slot-value server 'server-lock))
				(setf is-stop-server (not (eql +SERVER-STATE-RUNNING+ (slot-value server 'server-state))))
				(logger:log-message :debug "Popping queue")
				(setf next-command (queues:qpop (slot-value server 'command-queue)))
				(setf is-quit-commands (slot-value server 'quit-flag)))
			      (if is-stop-server
				  (progn
				    (bt:with-lock-held ((slot-value server 'server-lock))
				      (setf (slot-value server 'worker-state) +WORKER-STATE-TERMINATED+)
				      (setf (slot-value server 'server-state) +SERVER-STATE-STOPPED+))
				    (logger:log-message :debug "Worker thread: Stopped")
				    (return)))
			      (if (not next-command)
				  (progn
				    (bt:with-lock-held ((slot-value server 'server-lock))
				      (setf (slot-value server 'worker-state) +WORKER-STATE-IDLE+)
				      (setf (slot-value server 'quit-flag) nil))
				    (sleep 1))
				  (progn
				    (bt:with-lock-held ((slot-value server 'server-lock))
				      (setf (slot-value server 'worker-state)
					    (if is-quit-commands
						+WORKER-STATE-QUITTING+
						+WORKER-STATE-PROCESSING+
						)))
				    (let ((msg (execute-command server next-command)))
				      (bt:with-lock-held ((slot-value server 'server-lock))
					(save-send-message-no-lock server msg)))
				    )))))
		       :name "CfiServer-Worker-Thread"))))

(defun stop (server)
  "Stops the server. Does not aquire a lock."
  (cond
    ((eql +SERVER-STATE-INITIALIZED+ (slot-value server 'server-state))
     (setf (slot-value server 'server-state) +SERVER-STATE-STOPPED+))
    ((eql +SERVER-STATE-RUNNING+ (slot-value server 'server-state))
     (setf (slot-value server 'server-state) +SERVER-STATE-STOPPING+))))

(defmethod put ((server cfi-server) command)
  (with-lock-held ((slot-value server 'server-lock))
    (logger:log-message :debug (format nil "put: ~a" command))
    (cond
      ((string= command "ping")
       (save-send-message-no-lock server "pong"))
      ((string= command "quit")
       (setf (slot-value server 'quit-flag) t))
      ((string= command "stop")
       (stop server))
      ((string= command "start")
       (start server))
      (t
       (if (not (eql +SERVER-STATE-RUNNING+ (slot-value server 'server-state)))
	   (progn
	     (logger:log-message :error (format nil "put rejected because server is not running"))
	     (save-send-message-no-lock server (as-debug
						"Command rejected because server is not running, or stopping or stopped")))
	   (queues:qpush (slot-value server 'command-queue) command))))))



