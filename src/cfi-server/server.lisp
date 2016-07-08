 
(in-package :cfi-server)

(defclass cfi-server ()
  (
   (name :initarg :name :initform "OllisServer" :accessor name)
   (command-queue :initform (queues:make-queue :simple-cqueue) :accessor command-queue)
   (quit-flag :initform nil :accessor quit-flag)
   (quit-flag-lock :initform (bt:make-lock "quit-flag-lock") :accessor quit-flag-lock)
   (debug-flag :initform nil :accessor debug-flag)
   (started-flag :initform nil :accessor started-flag)
   (stopping-flag :initform nil :accessor stopping-flag)
   (stopping-flag-lock :initform (bt:make-lock "stopping-flag-lock") :accessor stopping-flag-lock)
   (put-lock :initform (bt:make-lock "put-lock") :accessor queue-lock)
   ))

(defgeneric put (cfi-server command)
  (:documentation "Add a command according to the Cfi-Specification to the queue."))

(defgeneric message (cfi-server message)
  (:documentation
   "Abstract callback handler to be implemented by a server.
    Is being called with strings according to the Cfi-Specification"))

(defgeneric start (cfi-server))
(defgeneric stop (cfi-server))
(defgeneric is-started (cfi-server))
(defgeneric is-stopping (cfi-server))
(defgeneric set-is-stopping (cfi-server flag))

(defun write-debug-message (server message)
  (if (slot-value server 'debug-flag)
      (message server (as-comment message))))

(defun next-command (server)
  (queues:qpop (slot-value server 'command-queue)))

(defparameter *backend-skip-cycles* 1000)

(defun create-backend-callback-handler (interval-seconds
					callback-fn
					&key
					  default-return-value
					  is-sticky-return-value)
  "Returns a wrapper function that filters high-frequency calls of 
the backend without using a lock. Filtered calls return the value 
of the initial/latest call of the callback function. The initial return
value is initialized by calling the callback function." 
  (let ((counter 0)
	(start-time (get-internal-real-time))
	(return-value default-return-value))
    (let ((fn (lambda ()
	       (setf counter (+ counter 1))
	       (if (> counter *backend-skip-cycles*)
		   (progn
		     (setf counter 0)
		     (let ((cur-time (get-internal-real-time)))
		       (if (>= (/ (- cur-time start-time) internal-time-units-per-second) interval-seconds)
			   (progn
			     (setf start-time cur-time)
			     (let ((result (funcall callback-fn)))
			       (if is-sticky-return-value (setf return-value result))))))))
	       return-value)))
      fn)))

(defmethod is-started ((server cfi-server))
  (slot-value server 'started-flag))

(defmethod is-stopping ((server cfi-server))
  (let ((stopping nil))
    (bt:with-lock-held ((slot-value server 'stopping-flag-lock))
      (setf stopping (slot-value server 'stopping-flag)))
    stopping))

(defmethod set-is-stopping ((server cfi-server) flag)
  (bt:with-lock-held ((slot-value server 'stopping-flag-lock))
      (setf (slot-value server 'stopping-flag) flag)))


(defmethod start ((server cfi-server))
  (if (is-stopping server)
      (message server (as-error "Server is shutting down"))
      (progn
	(if (is-started server)
	    (message server (as-error "Server already started"))
	    (progn
	      (setf (slot-value server 'started-flag) 1) 
	      (bt:make-thread (lambda ()
				(logger:log-message :info "Worker thread: Started")
				(loop
				   (if (is-stopping server)
				       (return))
				   (let ((cmd (next-command server)))
				     (if cmd
					 (invoke-command server cmd)
					 (progn
					   ;;(logger:log-message :info "Worker thread: Nothing to do. Going to sleep")
					   (release-quit server)
					   (sleep 1)))))
				(logger:log-message :info "Worker thread: Finished")
				(message server (as-comment "Worker thread: Stopped"))
				(set-is-stopping server nil)))
	      (message server "ready")
	      )))))


(defmethod stop ((server cfi-server))
  (if (is-stopping server)
      (message server (as-error "Server is shutting down"))
      (progn
	(if (not (is-started server))
	    (message server (as-error "Server not started"))
	    (progn 
	      (set-is-stopping server 1)
	      (setf (slot-value server 'started-flag) nil)
	      (loop
		 (if (not (is-stopping server))
		     (return)
		     (progn
		       (message server (as-comment "Stopping server..."))
		       (sleep 1)
		       )))
	      (message server (as-comment "Server stopped")))))))

(defun is-quitting (server)
  (let ((flag nil))
    (bt:with-lock-held ((slot-value server 'quit-flag-lock))
      (setf flag (slot-value server 'quit-flag)))
    flag))

(defun quit (server)
  (bt:with-lock-held ((slot-value server 'quit-flag-lock))
    (message server (as-comment "Set quit flag to true"))
    (setf (slot-value server 'quit-flag) t)))

(defun release-quit (server)
  ;; block to put commands into queue and check
  ;; if queue is empty.
  ;; if not, then reset the quit flag
  (bt:with-lock-held ((slot-value server 'put-lock))
    (if (= 0 (queues:qsize (slot-value server 'command-queue)))
	(bt:with-lock-held ((slot-value server 'quit-flag-lock))
	  (if (slot-value server 'quit-flag)
	      (progn 
		(message server (as-comment "Reset quit flag"))
		(setf (slot-value server 'quit-flag) nil)))))))

(defun as-comment (str)
  (format nil "# ~a" str))

(defun as-error (str)
  (format nil "# ~a" str))

   
(defun invoke-command (server command)
  (write-debug-message server (format nil "invoking command: ~a" command))
  (let ((msg (execute-command server command)))
    (write-debug-message server (format nil "returning result ~a for command: ~a" msg command))
    (message server msg)))

(defmethod put ((server cfi-server) command)
  (if (is-stopping server)
      (message server (as-error "Server is shutting down"))
      (progn
	(if (not (is-started server))
	    (message server (as-error "Server has not been started"))
	    (bt:with-lock-held ((slot-value server 'put-lock))
	      (logger:log-message :info (format nil "put: ~a" command))
	      (cond
		((string= command "ping")
		 (message server "pong"))
		((string= command "quit")
		 (quit server))
		(t
		   (queues:qpush (slot-value server 'command-queue) command))))))))



;;
;; Handlers
;;

(define-condition invalid-arguments (error)
  ((text :initarg :text :reader text)))

;; Parser wrapper. Catches all errors and raises server invalid-arguments condition
(defun parse (s parser)
  (handler-case
      (funcall parser s)
    (error (err)
      (progn
	(logger:log-message :error (format nil "parse failed: ~a" err))
	(error 'invalid-arguments :text (format nil "~a" s))))))

(defun quit-handler (server)
  (breaK)
  (message server "quit handler called")
  (quit server))

(defun play-handler (server board token depth &key column)
  (let ((parsed-board (parse board #'ccfi-placement-to-board))
	(quit-handler
	 (create-backend-callback-handler
	  2
	  (lambda ()
	    (message server (as-comment (format nil "IsQuit-Handler called. Quitting: ~a" (is-quitting server))))
	    (is-quitting server)
	    )
	  :default-return-value nil
	  :is-sticky-return-value 1
	  )))
    (format-play-result
     parsed-board
     (engine:play
      parsed-board
      (parse token #'ccfi-token-to-color)
      (parse-integer depth)
      :start-column (if column (parse-integer column) nil)
      :is-quit-fn quit-handler
      ))))
  
(defparameter *handler* 
  (list
   :play #'play-handler
   :quit  #'quit-handler
   ))


;;
;; Handler invoking stuff
;; TODO: Put code into a dedicated package
;;

(defun as-keyword (sym)
  (intern (string (string-upcase sym)) :keyword))

(defun get-handler (str)
  (getf *handler* (as-keyword str)))

(defun preprocess-parameter (parameter)
  (if (and (>= (length parameter) 3) (string= parameter "--" :start1 0 :end1 2 :start2 0 :end2 2))
      (as-keyword (subseq parameter 2))
      parameter))

(defun build-lambda-list (list)
  (mapcar #'preprocess-parameter list))

(defun invoke-command-handler (server name lambda-list)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (logger:log-message :debug (format nil "invoke-command-handler: ~a ~a" name lambda-list))
  (handler-case
      (let ((handler (get-handler name)))
	(if handler
	    (apply handler server lambda-list)
	    (as-error (format nil "Unknown command: ~a" name))))
    (invalid-arguments (err)
      (progn
	(logger:log-message :error (format nil "invoke-command-handler failed: ~a" err))
	(as-error (format nil "Command ~a called with invalid arguments" name))))
    (error (err)
      (progn
	(logger:log-message :error (format nil "invoke-command-handler failed: ~a" err))
	(as-error (format nil "Command ~a failed or called with insufficient arguments" name))))))


(defun execute-command (server cmdline)
  (logger:log-message :debug (format nil "execute-command: ~a" cmdline))
  (let* ((tokens (build-lambda-list (cl-ppcre:split "\\s" cmdline)))
	 (lambda-list (rest tokens))
	 (cmd (intern (string-upcase (car tokens)))))
    (invoke-command-handler server cmd lambda-list)))


