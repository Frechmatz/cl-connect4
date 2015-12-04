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
   (infoFn :initarg :infoFn)
   (shortInfoFn :initarg :shortInfoFn)
   (parseArgsFn :initarg :parseArgsFn)
   (execFn :initarg :execFn)
   )
   (:documentation "Interface of the commands that can be executed by the game repl")
  )

(defun print-help-text (command-table)
  "Prints an overview of the commands that can by entered into the game repl"
  (format t "Commands:~%help~%")
  (dolist (cmd command-table)
    (format t "~a~%"  (funcall (slot-value cmd 'infoFn))))
  (format t "~%")
  )

(defun print-help-text-short (command-table)
  "Prints an short overview of the commands that can by entered into the game repl"
  (format t "Commands: help")
  (labels ((inner (command-table first)
	     (format t (if first "~a" ", ~a")
		     (funcall (slot-value (first command-table) 'shortInfoFn)))
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
			      (funcall (slot-value opcode 'parseArgsFn) (cdr command-string) context)
			    (invalid-arguments (err) err)
			    )))
		    (if (listp parsed-args)
			(setf result (apply (slot-value opcode 'execFn) context parsed-args))
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
				       (push (make-instance
					      'command
					      :name "quit"
					      :infoFn (lambda () "quit: Quit game")
					      :shortInfoFn (lambda () "quit: Quit game")
					      :parseArgsFn (lambda (args context) (parse-arguments args '() context))
					      :execFn (lambda (context) (game-command-quit context))
					      ) table)
				       (push (make-instance
					      'command
					      :name "restart"
					      :shortInfoFn (lambda () "restart: To start a new game")
					      :infoFn (lambda () "restart: To start a new game")
					      :parseArgsFn (lambda (args context) (parse-arguments args '() context))
					      :execFn (lambda (context) (game-command-restart context) nil) ; quit loop by returning nil
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
	    (push (make-instance
		   'command
		   :name "play"
		   :shortInfoFn (lambda () "play <column>")
		   :infoFn (lambda () "play <column>: Play a move and get computers counter move. Column: 0..9 A..F")
		   :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-x) context))
		   :execFn (lambda (context x) (game-command-play-human context x))
		   ) table)
	    (push (make-instance
		   'command
		   :name "board"
		   :shortInfoFn (lambda () "board")
		   :infoFn (lambda () "board: Print current board")
		   :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		   :execFn (lambda (context) (game-command-print-board context))
		   ) table)
	    (push (make-instance
		   'command
		   :name "set-level"
		   :shortInfoFn (lambda () "set-level <n>")
		   :infoFn (lambda () "set-level <n>: Set the number of half-moves the computer will execute to determine it's best counter-move")
		   :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-level) context))
		   :execFn (lambda (context level) (game-command-set-level context level))
		   ) table)
	    (push (make-instance
		   'command
		   :name "set-board-size"
		   :shortInfoFn (lambda () "set-board-size <width> <height>")
		   :infoFn (lambda () "set-board-size <width> <height>: Set size of the board")
		   :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-board-dimension #'parse-board-dimension) context))
		   :execFn (lambda (context width height) (game-command-set-board-size context width height))
		   ) table)
	    (push (make-instance
		   'command
		   :name "hint"
		   :shortInfoFn (lambda () "hint")
		   :infoFn (lambda () "hint: Show next move the computer would do")
		   :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		   :execFn (lambda (context) (game-command-hint context))
		   ) table)
	    (push (make-instance
		   'command
		   :name "toggle-color"
		   :shortInfoFn (lambda () "toggle-color")
		   :infoFn (lambda () "toggle-color: Toggle the players color")
		   :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		   :execFn (lambda (context) (game-command-toggle-color context))
		   ) table)
	    (push (make-instance
		   'command
		   :name "continue"
		   :shortInfoFn (lambda () "continue")
		   :infoFn (lambda () "continue: Computer plays next move")
		   :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		   :execFn (lambda (context) (game-command-play-computer context))
		   ) table)
	    ;; (push (make-instance
	    ;;		   'command
	    ;;		   :name "set-board"
	    ;;		   :infoFn (lambda () "set-board <function>. <package-name>::<function-name> A function that returns a board.")
	    ;;		   :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-symbol) context))
	    ;;		   :execFn (lambda (context creator-fn) (game-command-set-board context creator-fn))
	    ;;		   ) table)
	    (push (make-instance
		    'command
		    :name "restart"
		    :shortInfoFn (lambda () "restart")
		    :infoFn (lambda () "restart: Restart game")
		    :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		    :execFn (lambda (context) (game-command-restart context))
		    ) table)
	    (push (make-instance
		    'command
		    :name "quit"
		    :shortInfoFn (lambda () "quit")
		    :infoFn (lambda () "quit: Quit game")
		    :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		    :execFn (lambda (context) (game-command-quit context))
		    ) table)

	    (nreverse table)
	    ))
	 (context
	  (let ((context (make-instance 'context)))
	    (setf (slot-value context 'board) (create-board CLASSIC-WIDTH CLASSIC-HEIGHT))
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


