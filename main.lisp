
#|

 #####                                           #       
#     #  ####  #    # #    # ######  ####  ##### #    #  
#       #    # ##   # ##   # #      #    #   #   #    #  
#       #    # # #  # # #  # #####  #        #   #    #  
#       #    # #  # # #  # # #      #        #   ####### 
#     # #    # #   ## #   ## #      #    #   #        #  
 #####   ####  #    # #    # ######  ####    #        #  

|#

(define-condition invalid-arguments (error)
  ((text :initarg :text :reader text)))

(define-condition quit-game (error)
  ((text :initarg :text :reader text)))

(defun find-element (list equalFn)
  (let ((elem (first list)))
    (if (or (not elem) (funcall equalFn elem)) elem (find-element (cdr list) equalFn))
    ))


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
;;
;; Command line parameter parsers
;;
;;

;; n: number in hex
(defun parse-number (n min-n max-n)
  (if (not (integerp n))
      (setf n (handler-case (parse-integer (format nil "~a" n) :radix 16)
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

;;
;; Parser for setting dimensions of the board
;; Values greater as 16 are not supported by the board formatter
;;
(defun parse-board-dimension (n  context)
  (declare (ignore context))
  (parse-number n 4 16))


;;
;;
;; Game commands
;; - may manipulate the context
;; - may print context status or messages
;;
;;

(defun game-command-set-board-size (context width height)
  (setf (slot-value context 'board) (create-board width height))
  (format-context context)
  )

(defun game-command-hint (context)
  (let ((result (best-move (slot-value context 'board) (slot-value context 'players-color) (slot-value context 'difficulty-level))))
    (format t "Recommended move is column ~a with a score of ~a" (first result) (third result))
    ))

(defun game-command-set-level (context level)
  (setf (slot-value context 'difficulty-level) level)
  (format-context context)
  )

(defun game-command-toggle-color (context)
  (setf (slot-value context 'players-color) (invert-color (slot-value context 'players-color)))
  (format-context context)
  )

(defun game-command-play-computer (context)
  (let (
	(computers-color (invert-color (slot-value context 'players-color)))
	)
    (game-command-play
     (slot-value context 'board) computers-color (slot-value context 'difficulty-level)
     (lambda (counter-x counter-y counter-score counter-board)
       (declare (ignore counter-score))
       (if (not counter-board)
	   (progn
	     (format-board (slot-value context 'board) *board-formatter*)
	     (format-message *message-formatter* "No counter move found")
	     )
	   (progn
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
		 )))))))

(defun game-command-play-human (context x)
  (let ((players-color (slot-value context 'players-color)))
    (game-command-throw-piece
     (slot-value context 'board) players-color x
     (lambda (x y board)
       (if (not board)
	   (format-message *message-formatter* "Invalid move. No place left in given column")
	   (progn
	     ;; Update context
	     (setf (slot-value context 'board) board)
	     (if (is-four board x y)
	       (progn
		 (format-board board *board-formatter* (max-line-at board x y players-color))
		 (format-message *message-formatter* "YOU ARE THE WINNER")
		 )
	       (game-command-play-computer context)
	       )))))))

;;
;; Print command overview
;;
(defun print-help-text (command-table)
  (format t "Commands:~%")
  (dolist (cmd command-table)
    (format t "~a~c"  (funcall (slot-value cmd 'infoFn)) #\newline)
    )
  (format t "q: quit~%r: restart game~%")
  )

;;
;; Print board and statuses
;;
(defun format-context (context &optional highlight-cells)
    (format-board (slot-value context 'board) *board-formatter* highlight-cells)
    (format t "~%Level: ~a Your color: ~a~%"
	    (slot-value context 'difficulty-level)
	    (format-cell-value *board-formatter* (slot-value context 'players-color)))
    )

(defun read-cmd ()
  (read-from-string (concatenate 'string "(" (read-line) ")"))
  )

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
;; - Set up the command table on which the game repl works
;; - Create the game context
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
		    :execFn (lambda (context)
			      (declare (ignore context))
			      ;; Signal quit
			      (error 'quit-game :text "Bye"))
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
		   :parseArgsFn (lambda (args context)
				  (parse-arguments args (list #'parse-x) context))
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

    
