
#|

 #####                                           #       
#     #  ####  #    # #    # ######  ####  ##### #    #  
#       #    # ##   # ##   # #      #    #   #   #    #  
#       #    # # #  # # #  # #####  #        #   #    #  
#       #    # #  # # #  # # #      #        #   ####### 
#     # #    # #   ## #   ## #      #    #   #        #  
 #####   ####  #    # #    # ######  ####    #        #  

|#

#|
Console based implementation of the Connect4 game
|#


;;
;; Condition definitions
;;

(define-condition invalid-arguments (error)
  ((text :initarg :text :reader text)))

(define-condition quit-game (error)
  ((text :initarg :text :reader text)))


;;
;; Output formatters
;;

(defclass message-formatter () ())
(defgeneric format-message (message-formatter message)
  (:documentation "Formats a text message"))
(defmethod format-message ( (formatter message-formatter) message)
  (format t "~a~%" message))
(defclass colorful-message-formatter (message-formatter) ())
(defmethod format-message ( (formatter colorful-message-formatter) message)
  (format t "~c[32m~a~c[0m~%" #\Esc message #\Esc))

(defvar *board-formatter*)
(defvar *message-formatter*)

;;
;; The Game Context
;;

(defparameter *GAME-STATE-CONNECTED-FOUR* 1)
(defparameter *GAME-STATE-NO-MOVES-LEFT* 2)
(defparameter *GAME-STATE-CONTINUE* 3)

(defclass context ()
  (
   (board :accessor board)
   (players-color :accessor players-color)
   (difficulty-level :accessor difficulty-level)
   (game-state :initarg :game-state :initform *GAME-STATE-CONTINUE* :accessor game-state)
   ))

;;
;;
;; Command line parameter parsers
;;

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

;; n: number in hex
(defun parse-number (n min-n max-n)
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
  (parse-number level 0 10))

(defun parse-color (c &optional context)
  (declare (ignore context))
  (if (equal c 'W) *WHITE*
    (if (equal c 'B) *BLACK*
      (error 'invalid-arguments :text "Invalid color. Valid colors are W and B")
      )))

(defun parse-board-dimension (n  context)
  (declare (ignore context))
  ;; board dimension > 16 not supported by formatter
  (parse-number n 4 16 ))


;;
;;
;; Game commands
;; - may manipulate the context
;; - may print context status or messages
;;
;;

(defun game-command-set-board-size (context width height)
  (setf (slot-value context 'board) (create-board width height))
  (format-context context))

(defun game-command-hint (context)
  (let ((result (best-move (slot-value context 'board) (slot-value context 'players-color) (slot-value context 'difficulty-level))))
    (format t "Recommended move is column ~a with a score of ~a" (first result) (third result))))

(defun game-command-set-level (context level)
  (setf (slot-value context 'difficulty-level) level)
  (format-context context))

(defun game-command-toggle-color (context)
  (setf (slot-value context 'players-color) (invert-color (slot-value context 'players-color)))
  (format-context context))

(defun game-command-play-computer (context)
  (let ((computers-color (invert-color (slot-value context 'players-color)))
	(counter-move nil) (counter-x nil) (counter-y nil) (counter-board nil))
    (setf counter-move (best-move (slot-value context 'board) computers-color (slot-value context 'difficulty-level)))
    (if (not counter-move)
	(format-message *message-formatter* "No counter move found")
	(progn
	  (setf counter-x (first counter-move))
	  (setf counter-y (second counter-move))
	  (setf counter-board (set-field (slot-value context 'board) counter-x counter-y computers-color))
	  (setf (slot-value context 'board) counter-board)
	  (if (is-four counter-board counter-x counter-y)
	      (progn 
		(format-board counter-board *board-formatter* (max-line-at counter-board counter-x counter-y computers-color))
		(format-message *message-formatter* "COMPUTER HAS WON")
		)
	      (progn
		(format-board counter-board *board-formatter* (list (list counter-x counter-y)))
		(format-message *message-formatter* (format nil "Computers move is ~a" counter-x))
		)
	      )))))

(defun game-command-play-human (context x)
  (let ((players-color (slot-value context 'players-color)) (y nil) (counter-board nil))
    (setf y (find-row (slot-value context 'board) x))
    (if (not y)
	(format-message *message-formatter* "Invalid move. No place left in given column")
	(progn
	  (setf counter-board (set-field (slot-value context 'board) x y players-color))
	  (setf (slot-value context 'board) counter-board)
	  (if (is-four counter-board x y)
	      (progn
		 (format-board counter-board *board-formatter* (max-line-at counter-board x y players-color))
		 (format-message *message-formatter* "YOU ARE THE WINNER")
		 )
	      (game-command-play-computer context)
	      )))))

(defun game-command-quit (context)
  (declare (ignore context))
  ;; Signal quit
  (error 'quit-game :text "Bye"))




;;
;; Print command overview
;;
(defun print-help-text (command-table)
  (format t "Commands:~%")
  (dolist (cmd command-table)
    (format t "~a~c"  (funcall (slot-value cmd 'infoFn)) #\newline))
  (format t "q: quit~%r: restart game~%"))

;;
;; Print board and statuses
;;
(defun format-context (context &optional highlight-cells)
    (format-board (slot-value context 'board) *board-formatter* highlight-cells)
    (format t "~%Level: ~a Your color: ~a~%"
	    (slot-value context 'difficulty-level)
	    (format-cell-value *board-formatter* (slot-value context 'players-color))))

;;
;; Read a command from the console
;;
(defun read-cmd ()
  (read-from-string (concatenate 'string "(" (read-line) ")"))
  )

;;
;; Helper for the lookup of commands
;;
(defun find-element (list equalFn)
  (let ((elem (first list)))
    (if (or (not elem) (funcall equalFn elem)) elem (find-element (cdr list) equalFn))
    ))

;;
;; Interface of commands executed by the game repl
;;

(defclass command ()
  (
   (name :initarg :name)
   (infoFn :initarg :infoFn)
   (parseArgsFn :initarg :parseArgsFn)
   (descriptionFn :initarg :descriptionFn)
   (execFn :initarg :execFn)
   (tags :initarg :tags)
   ))

;;
;; ****************************
;; Main game repl
;; ****************************
;;
(defun cmd-loop (context command-table)
  (let ((cmd nil) (opcode nil) (result nil))
    (format-context context)
    (princ #\newline)
    (print-help-text command-table)
    (labels ((do-command ()
	       (format t "Enter command: ")
	       (finish-output)
	       (setf cmd (read-cmd))
	       (princ #\newline)
	       (cond
		 ((equal (car cmd) '()) (print-help-text command-table) (setf result 'continue))
		 (t 
		  (setf opcode (find-element command-table (lambda (command) (equal (car cmd)  (slot-value command 'name)))))
		  (if opcode
		      (let ((parsed-args
			     (handler-case
				 (funcall (slot-value opcode 'parseArgsFn) (cdr cmd) context)
			       (invalid-arguments (err) err)
			       )))
			(if (listp parsed-args)
			    (apply (slot-value opcode 'execFn) context parsed-args)
			    ;; print parsing error
			    (format-message *message-formatter* (slot-value parsed-args 'text)))
			))))
	       (do-command)
	       ))
      (do-command)
      )))

;;
;; ********************************************************************
;; Start game
;; - Set up formatting functions for messages and the board
;; - Create the game context
;; - Set up the command table on which the game repl works
;; ********************************************************************
;;
(defun lets-play( &key (colors-not-supported t))
  (format t "~%~%Welcome to Connect4~%~%")
  (let ( (*board-formatter*
	  (if (not colors-not-supported)
	      (make-instance 'colorful-cell-formatter)
	      (make-instance 'cell-formatter))
	   )
	(*message-formatter*
	 (if (not colors-not-supported)
	     (make-instance 'colorful-message-formatter)
	     (make-instance 'message-formatter))
	  )
	 (command-table
	  (let ((table ()))
	    ;; Quit
	    (push (make-instance
		    'command
		    :name 'q
		    :infoFn (lambda () "q: Quit game")
		    :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		    :execFn (lambda (context) (game-command-quit context))
		    ) table)
	    ;; Print board
	    (push (make-instance
		   'command
		   :name 'board
		   :infoFn (lambda () "board: Print current board")
		   :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		   :execFn (lambda (context) (format-context context))
		   ) table)
	    ;; Set board size
	    (push (make-instance
		   'command
		   :name 'set-board-size
		   :infoFn (lambda () "set-board-size <width> <height>: Set size of the board")
		   :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-board-dimension #'parse-board-dimension) context))
		   :execFn (lambda (context width height) (game-command-set-board-size context width height))
		   ) table)
	    ;; Hint
	    (push (make-instance
		   'command
		   :name 'hint
		   :infoFn (lambda () "hint: Show next move the computer would do")
		   :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		   :execFn (lambda (context) (game-command-hint context))
		   ) table)
	    ;; Set Level
	    (push (make-instance
		   'command
		   :name 'set-level
		   :infoFn (lambda () "set-level <n>: Set the maximum traversal depth")
		   :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-level) context))
		   :execFn (lambda (context level) (game-command-set-level context level))
		   ) table)
	    ;; Play
	    (push (make-instance
		   'command
		   :name 'play
		   :infoFn (lambda () "play <column>: Play a move and get computers counter move. column can be entered in hex")
		   :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-x) context))
		   :execFn (lambda (context x) (game-command-play-human context x))
		   ) table)
	    ;; Toggle color
	    (push (make-instance
		   'command
		   :name 'toggle-color
		   :infoFn (lambda () "toggle-color: Toggle the players color")
		   :parseArgsFn (lambda (args context) (parse-arguments args '() context))
		   :execFn (lambda (context) (game-command-toggle-color context))
		   ) table)
	    table
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

    
