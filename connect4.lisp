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


(in-package :connect4)

;;;;
;;;; Output formatters
;;;;

(defclass message-formatter () ())
(defgeneric format-message (message-formatter message)
  (:documentation "Formats a text message"))
(defmethod format-message ( (formatter message-formatter) message)
  (format t "~a~%" message))
(defclass colorful-message-formatter (message-formatter) ()
  (:documentation "Formats a text message using ANSI escape sequences for colored output"))
(defmethod format-message ( (formatter colorful-message-formatter) message)
  (format t "~c[32m~a~c[0m~%" #\Esc message #\Esc))

(defvar *board-formatter*)
(defvar *message-formatter*)

;;;;
;;;; The Game Context
;;;;

(defconstant *GAME-STATE-FINAL* 1)
(defconstant *GAME-STATE-CONTINUE* 2)
(defconstant *GAME-STATE-PROCESSING-FINAL* 3)

(defclass context ()
  (
   (board :accessor board)
   (players-color :accessor players-color)
   (difficulty-level :accessor difficulty-level)
   (state :initarg :state :initform *GAME-STATE-CONTINUE* :accessor state)
   ))

;;;;
;;;; Command line argument parsers
;;;;

(define-condition invalid-arguments (error)
  ((text :initarg :text :reader text)))

(defun parse-arguments (args parsers context)
  (let ((result ()))
    (labels ((parse (args parsers context)
	       (let ((arg (car args)) (parser (car parsers)))
		 (cond 
		   ((and (not arg) (not parser)) nil)
		   ((and (not arg) parser) (error 'invalid-arguments :text "Missing arguments"))
		   ((and arg (not parser)) (error 'invalid-arguments :text "Too many arguments"))
		   (t 
		    (push (funcall parser arg context) result)
		    (parse (cdr args) (cdr parsers) context))
		   ))))
      (parse args parsers context))
    (reverse result)
    ))

(defun parse-number (n min-n max-n)
  "Parse a number out of a hex string."
  (if (not (integerp n))
      (setf n
	    (handler-case
		(parse-integer (format nil "~a" n) :radix 16)
	      (parse-error  nil))))
  (cond 
	 ((not (integerp n)) (error 'invalid-arguments :text "Not a number"))
	 ((> n max-n) (error 'invalid-arguments :text (format nil "Number too large: ~a. Allowed values are ~a...~a" n min-n max-n)))
	 ((< n min-n) (error 'invalid-arguments :text (format nil "Number too small: ~a. Allowed values are ~a...~a" n min-n max-n)))
	 (t n)
	 ))
  
(defun parse-x (x context)
  (parse-number x 0 (get-max-x (slot-value context 'board))))

(defun parse-y (y context)
  (parse-number y 0 (get-max-y (slot-value context 'board))))

(defun parse-level (level context)
  (declare (ignore context))
  (parse-number level 1 10))

(defun parse-color (c context)
  (declare (ignore context))
  (if (equal c 'W) *WHITE*
    (if (equal c 'B) *BLACK*
      (error 'invalid-arguments :text "Invalid color. Valid colors are W and B")
      )))

(defun parse-board-dimension (n  context)
  (declare (ignore context))
  ;; board dimension > 16 is not supported by the board-formatter
  (parse-number n 4 16 ))

(defun check-if-move-is-available (context)
  "Helper function to detect a final state"
  (if (not (is-move-available (slot-value context 'board)))
      (progn
	(format-message *message-formatter* "Draw! No more moves left.")
	(setf (slot-value context 'state) *GAME-STATE-FINAL*)
	)))

;;;;
;;;; Game commands
;;;;
;;;; - may modify the game context
;;;; - may print context statuses or messages
;;;;   to the console
;;;; - may return NIL to signal that the
;;;;   current command loop should be left.
;;;;   Used for exiting a nest command loop.
;;;;   Commands typically return t
;;;; - may indicate a final state by
;;;;   setting the game-state property of the
;;;;   game context to *GAME-STATE-FINAL*
;;;;

(define-condition quit-game (error)
  ((text :initarg :text :reader text)))

(defun game-command-set-board-size (context width height)
  (setf (slot-value context 'board) (create-board width height))
  (setf (slot-value context 'state) *GAME-STATE-CONTINUE*)
  (format-context context)
  t)

(defun game-command-hint (context)
  (let ((result (minmax (slot-value context 'board) (slot-value context 'players-color) (slot-value context 'difficulty-level))))
    (format-message *message-formatter* (format nil "Recommended move is column ~a with a score of ~a" (first result) (third result))))
  (setf (slot-value context 'state) *GAME-STATE-CONTINUE*)
  t)

(defun game-command-print-board (context)
  (format-context context)
  (setf (slot-value context 'state) *GAME-STATE-CONTINUE*)
  t)

(defun game-command-set-level (context level)
  (setf (slot-value context 'difficulty-level) level)
  (format-context context)
  (setf (slot-value context 'state) *GAME-STATE-CONTINUE*)
  t)

(defun game-command-toggle-color (context)
  (setf (slot-value context 'players-color) (invert-color (slot-value context 'players-color)))
  (format-context context)
  (setf (slot-value context 'state) *GAME-STATE-CONTINUE*)
  t)

(defun game-command-restart (context)
  (game-command-set-board-size context (get-board-width (slot-value context 'board)) (get-board-height (slot-value context 'board)))
  (format-context context)
  (format-message *message-formatter* "Restarted game")
  (setf (slot-value context 'state) *GAME-STATE-CONTINUE*)
  t)

(defun game-command-play-computer (context)
  (setf (slot-value context 'state) *GAME-STATE-CONTINUE*)
  (let ((computers-color (invert-color (slot-value context 'players-color)))
	(counter-move nil) (counter-x nil) (counter-y nil) (counter-board nil))
    (setf counter-move (minmax (slot-value context 'board) computers-color (slot-value context 'difficulty-level)))
    (if (not counter-move)
	(progn 
	  (format-message *message-formatter* "No counter move found")
	  (setf (slot-value context 'state) *GAME-STATE-FINAL*))  
	(progn
	  (setf counter-x (first counter-move))
	  (setf counter-y (second counter-move))
	  (setf counter-board (set-field (slot-value context 'board) counter-x counter-y computers-color))
	  (setf (slot-value context 'board) counter-board)
	  (if (is-four counter-board counter-x counter-y)
	      (progn 
		(format-context context (max-line-at counter-board counter-x counter-y computers-color))
		(format-message *message-formatter* (format nil "Computers move is ~a with a score of ~a" counter-x (third counter-move)))
		(format-message *message-formatter* "COMPUTER HAS WON")
		(setf (slot-value context 'state) *GAME-STATE-FINAL*)
		)
	      (progn
		(format-context context (list (list counter-x counter-y)))
		(format-message *message-formatter* (format nil "Computers move is ~a with a score of ~a" counter-x (third counter-move)))
		(check-if-move-is-available context)
		)
	      ))))
  t)

(defun game-command-play-human (context x)
  (setf (slot-value context 'state) *GAME-STATE-CONTINUE*)
  (let ((players-color (slot-value context 'players-color)) (y nil) (counter-board nil))
    (setf y (find-row (slot-value context 'board) x))
    (if (not y)
	(format-message *message-formatter* "Invalid move. No place left in given column")
	(progn
	  (setf counter-board (set-field (slot-value context 'board) x y players-color))
	  (setf (slot-value context 'board) counter-board)
	  (if (is-four counter-board x y)
	      (progn
		 (format-context context (max-line-at counter-board x y players-color))
		 (format-message *message-formatter* "YOU ARE THE WINNER")
		 (setf (slot-value context 'state) *GAME-STATE-FINAL*)
		 )
	      (game-command-play-computer context)
	      ))))
  t)

(defun game-command-quit (context)
  (declare (ignore context))
  ;; Signal quit. (todo: consider if this is a good idea)
  (error 'quit-game :text "Bye"))

(defclass command ()
  (
   (name :initarg :name)
   (infoFn :initarg :infoFn)
   (parseArgsFn :initarg :parseArgsFn)
   (execFn :initarg :execFn)
   )
   (:documentation "Interface of the commands that are executed by the game repl")
  )

;;;;
;;;; Helper functions
;;;;

(defun print-help-text (command-table)
  "Print command overview"
  (format t "Commands:~%")
  (dolist (cmd command-table)
    (format t "~a~c"  (funcall (slot-value cmd 'infoFn)) #\newline))
  (format t "~%")
  )

(defun format-context (context &optional highlight-cells)
  "Print board and statuses"
    (format-board *board-formatter* (slot-value context 'board) highlight-cells)
    (format t "~%Level: ~a Your color: ~a~%"
	    (slot-value context 'difficulty-level)
	    (format-cell-value *board-formatter* (slot-value context 'players-color))))

(defun read-cmd ()
  "Read a command from the console. Returns list of strings."
  (cl-ppcre:split "\\s" (read-line))
  )

(defun find-element (list equalFn)
  "Look up a command. todo: get rid of this function. replace it by functionality of the standard library"
  (let ((elem (first list)))
    (if (or (not elem) (funcall equalFn elem)) elem (find-element (cdr list) equalFn))
    ))

(defun do-cmd (context command-table command-string)
  "Execute a command"
  (let ((result t))
    (if (equal (car command-string) '())
	(progn
	  (format-context context)
	  (print-help-text command-table))
	(let ((opcode (find-element command-table (lambda (command) (equal (car command-string)  (slot-value command 'name))))))
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
	      (format t "Command not found~%")
	      )
	  ))
    result))
  

(defun cmd-loop (context command-table)
  "The game REPL"
  (let ((cmd nil))
    (format-context context)
    (princ #\newline)
    (print-help-text command-table)
    (labels ((do-command (command-table)
	       (format t "Enter command: ")
	       (finish-output)
	       (setf cmd (read-cmd))
	       (princ #\newline)
	       (if (do-cmd context command-table cmd)
		   (if (not (equal (slot-value context 'state) *GAME-STATE-FINAL*))
		       (do-command command-table)
		       (progn
			 ;; Process final state: Let player quit or restart game
			 (do-command (let ((table ()))
				       (push (make-instance
					      'command
					      :name "q"
					      :infoFn (lambda () "q: Quit game")
					      :parseArgsFn (lambda (args context) (parse-arguments args '() context))
					      :execFn (lambda (context) (game-command-quit context))
					      ) table)
				       (push (make-instance
					      'command
					      :name "r"
					      :infoFn (lambda () "r: To start a new game")
					      :parseArgsFn (lambda (args context) (parse-arguments args '() context))
					      :execFn (lambda (context) (game-command-restart context) nil) ; quit loop by returning nil
					      ) table)
				       ;; Prevent recursive entering into final state processing
				       (setf (slot-value context 'state) *GAME-STATE-PROCESSING-FINAL*)
				       table))
			 (do-command command-table))
		       )
		   )))
      (do-command command-table)
      )))


;;;;
;;;; ***********************************************************************************
;;;; Start the game
;;;; - Set up formatting contexts for messages and the board
;;;; - Create the game context
;;;; - Set up the command table on which the game repl will work
;;;; ***********************************************************************************
;;;;
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
		   :infoFn (lambda () "play <column>: Play a move and get computers counter move. Column: 0..9 A..F")
		   :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-x) context))
		   :execFn (lambda (context x) (game-command-play-human context x))
		   ) table)
	    (push (make-instance
		   'command
		   :name "board"
		   :infoFn (lambda () "board: Print current board")
		   :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		   :execFn (lambda (context) (game-command-print-board context))
		   ) table)
	    (push (make-instance
		   'command
		   :name "set-level"
		   :infoFn (lambda () "set-level <n>: Set the number of half-moves the computer will execute to determine it's best counter-move")
		   :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-level) context))
		   :execFn (lambda (context level) (game-command-set-level context level))
		   ) table)
	    (push (make-instance
		   'command
		   :name "set-board-size"
		   :infoFn (lambda () "set-board-size <width> <height>: Set size of the board")
		   :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-board-dimension #'parse-board-dimension) context))
		   :execFn (lambda (context width height) (game-command-set-board-size context width height))
		   ) table)
	    (push (make-instance
		   'command
		   :name "hint"
		   :infoFn (lambda () "hint: Show next move the computer would do")
		   :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		   :execFn (lambda (context) (game-command-hint context))
		   ) table)
	    (push (make-instance
		   'command
		   :name "toggle-color"
		   :infoFn (lambda () "toggle-color: Toggle the players color")
		   :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		   :execFn (lambda (context) (game-command-toggle-color context))
		   ) table)
	    (push (make-instance
		   'command
		   :name "continue"
		   :infoFn (lambda () "continue: Computer plays next move")
		   :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		   :execFn (lambda (context) (game-command-play-computer context))
		   ) table)
	    (push (make-instance
		    'command
		    :name "r"
		    :infoFn (lambda () "r: Restart game")
		    :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		    :execFn (lambda (context) (game-command-restart context))
		    ) table)
	    (push (make-instance
		    'command
		    :name "q"
		    :infoFn (lambda () "q: Quit game")
		    :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		    :execFn (lambda (context) (game-command-quit context))
		    ) table)
	    (nreverse table)
	    ))
	 (context
	  (let ((context (make-instance 'context)))
	    (setf (slot-value context 'board) (create-board *CLASSIC-WIDTH* *CLASSIC-HEIGHT*))
	    (setf (slot-value context 'players-color) 'W)
	    (setf (slot-value context 'difficulty-level) 4)
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

