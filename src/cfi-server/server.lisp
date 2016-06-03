 
(in-package :cfi-server)

(defclass cfi-server ()
  (
   (name :initarg :name :initform "OllisServer" :accessor name)
   (command-queue :initform (queues:make-queue :simple-queue) :accessor command-queue)
   (quit-flag :initform nil :accessor quit-flag)
   (debug-flag :initform nil :accessor debug-flag)
   (current-command :initform nil :accessor current-command)
   (disconnected-flag :initform nil :accessor disconnected-flag)
   ))

(defgeneric put-command (cfi-server command)
  (:documentation "Add a command to the comamnd queue"))
(defgeneric write-message (cfi-server message))
(defgeneric start (cfi-server))
(defgeneric stop (cfi-server))

(defmethod start ((server cfi-server))
  (write-message server (format nil "ready")))

(defmethod stop ((server cfi-server))
  (setf (slot-value server 'disconnected-flag) t))

(defun is-quitting (server)
  (slot-value server 'quit-flag))

(defun quit (server)
  (setf (slot-value server 'quit-flag) t))

(defun is-current-command (server)
  (slot-value server 'current-command))

(defun set-current-command (server cmd)
  (setf (slot-value server 'current-command) cmd))

(defun as-comment (str)
  (format nil "# ~a" str))

(defun as-error (str)
  (format nil "# ~a" str))

(defun write-debug-message (server message)
  (if (slot-value server 'debug-flag)
      (write-message server (as-comment message))))

(defun try-invoke-next-command (server)
  (if (is-current-command server)
      nil
      (progn
	(let ((cmd (queues:qpop (slot-value server 'command-queue))))
	  (if cmd
	      (invoke-command server cmd)
	      )))))

   
(defun invoke-command (server command)
  (write-debug-message server (format nil "invoking command: ~a" command))
  (set-current-command server command)
  (let ((msg (execute-command server command)))
    (write-debug-message server (format nil "returning result ~a for command: ~a" msg command))
    (write-message server msg))
  (set-current-command server nil)
  (try-invoke-next-command server))

(defmethod put-command ((server cfi-server) command)
  (logger:log-message :info (format nil "put-command: ~a" command))
  (if (equal command "ping")
      (write-message server "pong")
      (progn
	(queues:qpush (slot-value server 'command-queue) command)
	(try-invoke-next-command server))))


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


