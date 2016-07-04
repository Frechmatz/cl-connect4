 
(in-package :cfi-server)

(defclass cfi-server ()
  (
   (name :initarg :name :initform "OllisServer" :accessor name)
   (command-queue :initform (queues:make-queue :simple-cqueue) :accessor command-queue)
   (quit-flag :initform nil :accessor quit-flag)
   (debug-flag :initform nil :accessor debug-flag)
   (started-flag :initform nil :accessor started-flag)
   (stopping-flag :initform nil :accessor stopping-flag)
   (stopping-flag-lock :initform (bt:make-lock "stopping-flag-lock") :accessor stopping-flag-lock)
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
					   (sleep 1)))))
				(logger:log-message :info "Worker thread: Finished")
				(write-debug-message server "Worker thread: Finished")
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
  (slot-value server 'quit-flag))

(defun quit (server)
  (setf (slot-value server 'quit-flag) t))

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
	    (progn
	      (logger:log-message :info (format nil "put: ~a" command))
	      (if (equal command "ping")
		  (message server "pong")
		    (queues:qpush (slot-value server 'command-queue) command)))))))


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
  (quit server))

(defun play-handler (server board token depth &key column)
  (let ((parsed-board (parse board #'ccfi-placement-to-board)))
    (format-play-result
     parsed-board
     (engine:play
      parsed-board
      (parse token #'ccfi-token-to-color)
      (parse-integer depth)
      :start-column (if column (parse-integer column) nil)
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
  ;; (declare (optimize (debug 3) (speed 0) (space 0)))
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


