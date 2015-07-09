
#|

 #####                                           #       
#     #  ####  #    # #    # ######  ####  ##### #    #  
#       #    # ##   # ##   # #      #    #   #   #    #  
#       #    # # #  # # #  # #####  #        #   #    #  
#       #    # #  # # #  # # #      #        #   ####### 
#     # #    # #   ## #   ## #      #    #   #        #  
 #####   ####  #    # #    # ######  ####    #        #  
                                                         
|#



(load "field.lisp")
(load "boardformatter.lisp")
(load "context.lisp")
(load "argparser.lisp")
(load "command.lisp")
(load "commandresult.lisp")
(load "classic.lisp")
(load "gamecommands.lisp")

(define-condition invalid-arguments (error)
  ((text :initarg :text :reader text)))

(defun find-element (list equalFn)
  (let ((elem (first list)))
    (if (or (not elem) (funcall equalFn elem)) elem (find-element (cdr list) equalFn))
    ))

;; n: number 0..9 A..Z a..z (a == 10, b == 11, ...)
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
;; UI bindings of game commands
;;
(defun create-command-table ()
  (let ((table ()))

    (push (make-instance 'command
			 :name 'board
			 :infoFn (lambda () "board: Print current board")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args context) (parse-arguments args '() context))
			 :execFn #'game-command-board
			 ) table)

    (push (make-instance 'command
			 :name 'put
			 :infoFn (lambda () "put color <column> <row>: Put a piece into the board. column and row can be entered in hex")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-color #'parse-x #'parse-y) context))
			 :tags (list "DEVELOPER")
			 :execFn #'game-command-put
			 ) table)

    (push (make-instance 'command
			 :name 'set-board-size
			 :infoFn (lambda () "set-board-size <width> <height>: Set size of the board")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-board-dimension #'parse-board-dimension) context))
			 :tags (list "DEVELOPER" "PLAYER")
			 :execFn #'game-command-set-board-size
			 ) table)

    (push (make-instance 'command
			 :name 'hint
			 :infoFn (lambda () "hint: Show next move the computer would do")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args context) (parse-arguments args '() context))
			 :execFn #'game-command-hint
			 ) table)

    (push (make-instance 'command
			 :name 'is-four
			 :infoFn (lambda () "is-four <column> <row>: Check if four pieces are in a row at given position. column and row can be entered in hex")
			 :tags (list "DEVELOPER")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-x #'parse-y) context))
			 :execFn #'game-command-is-four
			 ) table)

    (push (make-instance 'command
			 :name 'set-level
			 :infoFn (lambda () "set-level <n>: Set the maximum traversal depth")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-level) context))
			 :execFn #'game-command-set-level
			 ) table)

    (push (make-instance 'command
			 :name 'play
			 :infoFn (lambda () "play <column>: Play a move and get computers counter move. column can be entered in hex")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-x) context))
			 :execFn #'game-command-play
			 ) table)

  (push (make-instance 'command
			 :name 'continue
			 :infoFn (lambda () "continue: Let the computer make a move")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args context) (parse-arguments args '() context))
			 :execFn #'game-command-continue
			 ) table)

    (push (make-instance 'command
			 :name 'toggle-color
			 :infoFn (lambda () "toggle-color: Toggle the players color")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args context) (parse-arguments args '() context))
			 :execFn #'game-command-toggle-color
			 ) table)

    table
    ))


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
    (format-board (slot-value context 'board) (slot-value context 'cell-formatter) highlight-cells)
    (format t "~%Level: ~a Your color: ~a~%"
	    (slot-value context 'difficulty-level)
	    (format-cell-value (slot-value context 'cell-formatter) (slot-value context 'players-color)))
    )

(defun read-cmd ()
  (read-from-string (concatenate 'string "(" (read-line) ")"))
  )

;;
;; Execute a command entered into the game repl
;;
(defun exec-command (opcode context args)
  (let ((parsed-args (handler-case (funcall (slot-value opcode 'parseArgsFn) args context)
					 (invalid-arguments (err) err)
					 )))
    (if (listp parsed-args)
	(apply (slot-value opcode 'execFn) context parsed-args)
      (make-instance 'command-result :redraw-board nil :message (slot-value parsed-args 'text)))
    ))

;;
;; ****************************
;; Main game repl
;; ****************************
;;
(defun cmd-loop (context-factory)
  (let ((context (funcall context-factory)))
    (let ( (cmd nil) (opcode nil) (result nil) (command-table (slot-value context 'command-table)))
      (format-context context)
      (princ #\newline)
      (print-help-text command-table)
      (flet ((do-command ()
			 (format t "Enter command: ")
			 ;; http://stackoverflow.com/questions/8360386/sbcl-switches-print-and-read-order-lisp
			 ;; flush output stream to console
			 (finish-output)
			 (setf cmd (read-cmd))
			 (princ #\newline)
			 (cond
			  ((equal (car cmd) '()) (print-help-text command-table) (setf result 'continue))
			  ((equal (car cmd) 'q) (format t "Bye.~%Enter (quit) to exit lisp~%") (setf result nil))
			  ((equal (car cmd) 'r)
			   (setf context (funcall context-factory))
			   (format-context context)
			   (setf result 'continue)) 
			  (t 
			   (setf opcode (find-element command-table (lambda (command) (equal (car cmd)  (slot-value command 'name)))))
			   (if opcode
			       (setf result (exec-command opcode context (cdr cmd)))
			     (setf result (make-instance 'command-result :redraw-board nil :message nil))
			     )
			   (if (slot-value result 'redraw-board)
			       (format-context context (slot-value result 'highlight-cells)))
			   (if (slot-value result 'message) (progn (princ (slot-value result 'message)) (princ #\newline)))
			   )
			  )
			 result))
	    (do ((ergebnis (do-command) (do-command)))
		((not ergebnis))
		)
	    )
      )
    )	  
  )

(defun create-default-context ()
  (let ((context (make-instance 'context)))
    (setf (slot-value context 'board) (create-board *CLASSIC-WIDTH* *CLASSIC-HEIGHT*))
    (setf (slot-value context 'players-color) 'W)
    (setf (slot-value context 'cell-formatter) (make-instance 'colorful-cell-formatter))
    (setf (slot-value context 'command-table) (create-command-table))
    (setf (slot-value context 'difficulty-level) 4)
    (setf (slot-value context 'format-alert-message)
	  (lambda (msg)
	    (format nil "~c[32m~a~c[0m" #\Esc msg #\Esc))) 
    context
    ))

;;
;; ****************************
;; Start game in developer mode
;; ****************************
;;
(defun lets-go()
  (cmd-loop (lambda ()
 	      (format t "~%~%Welcome to Connect4~%~%")
	      (create-default-context))))

;;
;; *************************
;; Start game in player mode
;; *************************
;;
(defun lets-play()
  (cmd-loop (lambda ()
 	      (format t "~%~%Welcome to Connect4~%~%")
	      (let ((context (create-default-context)))
		;; filter away developer commands
		(setf (slot-value context 'command-table)
		      (remove-if-not (lambda (cmd)
				       (find "PLAYER" (slot-value cmd 'tags) :test #'equal))
				     (slot-value context 'command-table)))
		context
		))))

