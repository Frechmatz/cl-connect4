;;
;; Command implementations
;; 
;;


(in-package :cfi-server)

;; Command lookup table
(defparameter *handler* '())

(defun add-handler (symbol fn)
  (push fn *handler*)
  (push symbol *handler*))



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


(defun create-lazy-quit-play-handler (server)
  (create-lazy-handler
   2 ;; every two seconds
   (lambda ()
     (let ((is-quitting nil))
       (bt:with-lock-held ((slot-value server 'server-lock))
	 (setf is-quitting (or
			    (slot-value server 'quit-flag)
			    (not (eql +SERVER-STATE-RUNNING+ (slot-value server 'server-state))))))
       (if is-quitting
	   (logger:log-message :info "Send quit signal to engine"))
       is-quitting))
   :default-return-value nil
   :is-sticky-return-value t
   ))
  
(defun create-lazy-info-handler (server)
  (create-lazy-handler
   10 ;; every 10 seconds
   (lambda ()
     (lambda (info)
       (save-send-message-with-lock server (format-info info))))
   :default-return-value nil
   :is-sticky-return-value nil
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
    (let ((result (invoke-command-handler server cmd lambda-list)))
      (logger:log-message :debug (format nil "Returning result ~a for command: ~a" result cmdline))
      result)))




;;
;; Handlers
;;

(defun play-handler (server board token depth &key column)
  (let ((parsed-board (parse board #'ccfi-placement-to-board)))
    (format-play-result
     parsed-board
     (engine:play
      parsed-board
      (parse token #'ccfi-token-to-color)
      (parse depth #'parse-integer)
      :start-column (if column (parse column #'parse-integer) nil)
      :is-quit-fn (create-lazy-quit-play-handler server)
      :info-fn (create-lazy-info-handler server)
      ))))
  
(add-handler :play #'play-handler)

