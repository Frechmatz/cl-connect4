;;;;
;;;;   #####                                           #       
;;;;  #     #  ####  #    # #    # ######  ####  ##### #    #  
;;;;  #       #    # ##   # ##   # #      #    #   #   #    #  
;;;;  #       #    # # #  # # #  # #####  #        #   #    #  
;;;;  #       #    # #  # # #  # # #      #        #   ####### 
;;;;  #     # #    # #   ## #   ## #      #    #   #        #  
;;;;   #####   ####  #    # #    # ######  ####    #        #  
;;;;  
;;;;  A console based implementation of the Connect Four game
;;;;


(in-package :connect4-console)

;;;
;;; The game repl specific code 
;;;

(defclass command ()
  (
   (name :initarg :name)
   (info :initarg :info)
   (short-info :initarg :short-info)
   (parse-args-fn :initarg :parse-args-fn)
   (exec-fn :initarg :exec-fn)
   )
   (:documentation "Interface of the commands that can be executed by the game repl")
  )

(defun create-command (name short-info long-info parser-list target-fn)
  (make-instance
   'command
   :name name
   :short-info short-info
   :info long-info
   :parse-args-fn (lambda (context args) (parse-arguments args parser-list context))
   :exec-fn (lambda (context &rest the-rest) (apply target-fn context the-rest))
   ))

(defun create-help-text (command &key (short nil))
  (if short
      (slot-value command 'short-info)
      (slot-value command 'info)
  ))
  
(defun print-help-text (command-table)
  "Prints an overview of the commands that can by entered into the game repl"
  (format t "Commands:~%help~%")
  (dolist (cmd command-table)
    (format t "~a~%" (create-help-text cmd :short nil)))
  (format t "~%")
  )

(defun print-help-text-short (command-table)
  "Prints an short overview of the commands that can by entered into the game repl"
  (format t "Commands: help")
  (labels ((inner (command-table first)
	     (format t (if first "~a" ", ~a")
		     (create-help-text (first command-table) :short 1))
	     (if (> (length command-table) 1)
		 (inner (cdr command-table) nil))))
    (inner command-table nil))
  (format t "~%")
  )

(defun read-cmd ()
  "Read a command from the console. Returns list of strings."
  (cl-ppcre:split "\\s" (read-line))
  )

(defun do-cmd (context command-table command-string)
  "Executes a command entered via the game repl. command-string: list of strings, consisting of the command and additional arguments."
  (let ((result t) (cmd (car command-string)))
    (if (equal cmd '())
	(progn
	  (format-context context)
	  (print-help-text-short command-table))
	(if (equal cmd "help")
	    (print-help-text command-table)
	    (let ((opcode (find-if (lambda (c) (equal cmd (slot-value c 'name))) command-table)))
	      (if opcode
		  (let ((parsed-args
			  (handler-case
			      (funcall (slot-value opcode 'parse-args-fn) context (cdr command-string))
			    (invalid-arguments (err) err)
			    )))
		    (if (listp parsed-args)
			(setf result (apply (slot-value opcode 'exec-fn) context parsed-args))
			;; print parsing error
			(format-message *message-formatter* (slot-value parsed-args 'text))
			))
		  (format t "Command not found: ~a~%" cmd)
		  )
	      )))
	result))
  

(defun cmd-loop (context command-table)
  "The game repl"
  (let ((cmd nil))
    (format-context context)
    (princ #\newline)
    (print-help-text-short command-table)
    (labels ((do-command (command-table)
	       (format t "Enter command: ")
	       (finish-output)
	       (setf cmd (read-cmd))
	       (princ #\newline)
	       (if (do-cmd context command-table cmd)
		   (if (not (equal (slot-value context 'state) GAME-STATE-FINAL))
		       (do-command command-table)
		       (progn
			 ;; Process final state: Let player quit or restart game
			 (do-command (let ((table ()))
				       (push
					(create-command
					 "quit"
					 "quit: Quit game"
					 "quit"
					 '()
					 #'game-command-quit) table)
				       (push
					(create-command
					 "restart"
					 "restart"
					 "restart: Start new game"
					 '()
					 (lambda (context) (game-command-restart context) nil) ; quit loop by returning nil
					 ) table)
				       ;; Prevent recursive entering into final state processing
				       (setf (slot-value context 'state) GAME-STATE-PROCESSING-FINAL)
				       table))
			 (do-command command-table))
		       )
		   )))
      (do-command command-table)
      )))

;;;
;;; ***********************************************************************************
;;; Start the game
;;; - Set up formatting contexts for messages and the board
;;; - Create the game context
;;; - Set up the command table on which the game repl will work
;;; ***********************************************************************************
;;;
(defun lets-play( &key (colors-not-supported t))
  "Starts the game"
  (format t "~%~%Welcome to Connect4~%~%")
  (let ( (*board-formatter*
	  (if (not colors-not-supported)
	      (make-instance 'colorful-board-formatter)
	      (make-instance 'board-formatter))
	   )
	(*message-formatter*
	 (if (not colors-not-supported)
	     (make-instance 'colorful-message-formatter)
	     (make-instance 'message-formatter))
	  )
	 (command-table
	  (let ((table ()))
	    (push 
	     (create-command
	      "play"
	      "play <column>"
	      "play <column>: Play a move and get computers counter move. Column: 0..9 A..F"
	      (list #'parse-x)
	      #'game-command-play-human) table)
	    
	    (push
	     (create-command
	      "board"
	      "board"
	      "board: Print current board"
	      '()
	      #'game-command-print-board) table)

	    (push
	     (create-command
	      "set-level"
	      "set-level <n>"
	      "set-level <n>: Set the number of half-moves the computer will execute to determine it's best counter-move"
	      (list #'parse-level)
	      #'game-command-set-level) table)
	    
	    (push
	     (create-command
	      "set-board-size"
	      "set-board-size <width> <height>"
	      "set-board-size <width> <height>: Set size of the board"
	      (list #'parse-board-dimension #'parse-board-dimension)
	      #'game-command-set-board-size) table)
	    
	    (push
	     (create-command
	      "hint"
	      "hint"
	      "hint: Show next move the computer would do"
	      '()
	      #'game-command-hint) table)
	    
	    (push
	     (create-command
	      "toggle-color"
	      "toggle-color"
	      "toggle-color: Toggle the players color"
	      '()
	      #'game-command-toggle-color) table)
	    
	    (push
	     (create-command
	      "continue"
	      "continue"
	      "continue: Computer plays next move"
	      '()
	      #'game-command-play-computer) table)

	    (push
	     (create-command
	      "restart"
	      "restart"
	      "restart: Restart game"
	      '()
	      #'game-command-restart) table)
	    
	    (push
	     (create-command
	      "quit"
	      "quit"
	      "quit: Quit game"
	      '()
	      #'game-command-quit) table)

	    (nreverse table)
	    ))
	 (context
	  (let ((context (make-instance 'context)))
	    (setf (slot-value context 'board) (create-board engine:CLASSIC-WIDTH engine:CLASSIC-HEIGHT))
	    (setf (slot-value context 'players-color) board:WHITE)
	    (setf (slot-value context 'difficulty-level) 6)
	    context))
	 )
    (handler-case (cmd-loop context command-table)
      (quit-game (info)
	(declare (ignore info))
	nil))
    (format t "Bye. Thanks for playing.~%")
    ))

(defun lets-play-colorful ()
  "Starts the game using ANSI escape sequences for colored output"
  (lets-play :colors-not-supported nil))
(defun lpc ()
  "Shortcut for lets-play-colorful"
  (lets-play-colorful))


