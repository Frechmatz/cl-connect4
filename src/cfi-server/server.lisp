
;;
;; Implementation of the server-class methods
;; Handles all the state transitions
;;


(in-package :cfi-server)

(defmethod start ((server cfi-server))
  (bt:with-lock-held ((slot-value server 'server-lock))
    (cond
      ((not (eql +SERVER-STATE-INITIALIZED+ (slot-value server 'server-state)))
       (logger:log-message :error "A server cannot be re-started"))
      (t
       (setf (slot-value server 'server-state) +SERVER-STATE-RUNNING+)
       (bt:make-thread (lambda ()
			 (logger:log-message :info "Worker thread: Started")
			 (loop
			    (let ((is-stop-server nil) (next-command nil) (is-quit-commands nil))
			      (bt:with-lock-held ((slot-value server 'server-lock))
				(setf is-stop-server (not (eql +SERVER-STATE-RUNNING+ (slot-value server 'server-state))))
				(setf next-command (queues:qpop (slot-value server 'command-queue)))
				(setf is-quit-commands (slot-value server 'quit-flag)))
			      (if is-stop-server
				  (progn
				    (bt:with-lock-held ((slot-value server 'server-lock))
				      (setf (slot-value server 'worker-state) +WORKER-STATE-TERMINATED+))
				    (logger:log-message :info "Worker thread: Stopped")
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
				    (message server (execute-command server next-command))
				    ))))))))))

(defmethod stop ((server cfi-server))
  (let ((wait-for-stop nil))
    (bt:with-lock-held ((slot-value server 'server-lock))
      (if (eql +SERVER-STATE-RUNNING+ (slot-value server 'server-state))
	  (progn
	    (setf wait-for-stop t)
	    (setf (slot-value server 'server-state) +SERVER-STATE-STOPPING+))))
    (if wait-for-stop
	(loop
	   (message server (as-comment "Stopping server..."))
	   (sleep 1)
	   (let ((stopped nil))
	     (bt:with-lock-held ((slot-value server 'server-lock))
	       (setf stopped (if (eql +WORKER-STATE-TERMINATED+ (slot-value server 'worker-state))
				 t
				 nil)))
	     (if stopped
		 (progn
		   (bt:with-lock-held ((slot-value server 'server-lock))
		     (setf (slot-value server 'server-state) +SERVER-STATE-STOPPED+))
		   (return)))
	     )))))

(defmethod put ((server cfi-server) command)
  (with-lock-held ((slot-value server 'server-lock))
    (if (not (eql +SERVER-STATE-RUNNING+ (slot-value server 'server-state)))
	(message server (as-error "Server not started or shutting down"))
	(progn
	  (logger:log-message :info (format nil "put: ~a" command))
	  (cond
	    ((string= command "ping")
	     (message server "pong"))
	    ((string= command "quit")
	     (setf (slot-value server 'quit-flag) t))
	    (t
	     (queues:qpush (slot-value server 'command-queue) command)))))))



