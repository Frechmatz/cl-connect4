
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
(load "context.lisp")
(load "argparser.lisp")
(load "command.lisp")
(load "commandresult.lisp")
(load "classic.lisp")


(define-condition invalid-arguments (error)
  ((text :initarg :text :reader text)))

(defun find-element (list equalFn)
  (let ((elem (first list)))
    (if (or (not elem) (funcall equalFn elem)) elem (find-element (cdr list) equalFn))
    ))

;;
;; Parser for row/column-numbers representing a field of the board
;; n: number 0..9 A..Z a..z (a == 10, b == 11, ...)
;; Todo: Add support for rows/columns represented by characters, for example a == 10
;; 
(defun parse-xy (n min-n max-n)
  (cond 
	 ((not (integerp n)) (error 'invalid-arguments :text "Not a number"))
	 ((> n max-n) (error 'invalid-arguments :text (format nil "Number too large: ~a. Allowed values are ~a...~a" n min-n max-n)))
	 ((< n min-n) (error 'invalid-arguments :text (format nil "Number too small: ~a. Allowed values are ~a...~a" n min-n max-n)))
	 (t n)
	 ))

(defun parse-x (x context)
  (parse-xy x 0 (get-max-x (slot-value context 'board))))

(defun parse-y (y context)
  (parse-xy y 0 (get-max-y (slot-value context 'board))))

(defun parse-level (level context)
  (cond 
	 ((not (integerp level)) (error 'invalid-arguments :text "Not a number"))
	 ((< level 0) (error 'invalid-arguments :text (format nil "Level value too small. Allowed values are 0...n")))  
	 (t level)
	 ))

(defun parse-color (c context)
  (if (equal c 'W) *WHITE*
    (if (equal c 'B) *BLACK*
      (error 'invalid-arguments :text "Invalid color. Valid colors are W and B")
      )))

;;
;; Parser for setting dimensions of the board
;; Values greater as 10 are currently not supported by the board formatter
;;
(defun parse-board-dimension (n context)
  (cond 
	 ((not (integerp n)) (error 'invalid-arguments :text "Not a number"))
	 ((> n 10) (error 'invalid-arguments :text (format nil "Number too large: ~a. Allowed values are 4..10" n)))
	 ((< n 4) (error 'invalid-arguments :text (format nil "Number too small: ~a. Allowed values are 4..10" n)))
	 (t n)
	 ))

;; Command table for the game repl
(defun create-command-table ()
  (let ((table ()))

    (push (make-instance 'command
			 :name 'board
			 :infoFn (lambda () "board: Print current board")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args context) (parse-arguments args '() context))
			 :execFn (lambda (context)
				   (make-instance 'command-result :redraw-board t :message nil)
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'put
			 :infoFn (lambda () "put color <column> <row>: Put a piece into the board")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-color #'parse-x #'parse-y) context))
			 :tags (list "DEVELOPER")
			 :execFn (lambda (context color x y)
				   (let ((board (slot-value context 'board)))
				     (setf board (set-field board x y color))
				     (setf (slot-value context 'board) board)
				     (make-instance 'command-result :redraw-board t :message nil)
				     )
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'set-board-size
			 :infoFn (lambda () "set-board-size <width> <height>: Set size of the board")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-board-dimension #'parse-board-dimension) context))
			 :tags (list "DEVELOPER" "PLAYER")
			 :execFn (lambda (context width height)
				   (let ((new-board (create-board width height)))
				     (setf (slot-value context 'board) new-board)
				     (make-instance 'command-result :redraw-board t :message nil)
				     )
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'hint
			 :infoFn (lambda () "hint <color>: Show next move the computer would do")
			 :tags (list "DEVELOPER")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-color) context))
			 :execFn (lambda (context color)
				   (let ((board (slot-value context 'board)))
				     (let ((result (best-move board color (slot-value context 'difficulty-level))))
				       (make-instance 'command-result :redraw-board t :message result)))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'is-four
			 :infoFn (lambda () "is-four <column> <row>: Check if four pieces are in a row at the given position")
			 :tags (list "DEVELOPER")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-x #'parse-y) context))
			 :execFn (lambda (context x y)
				   (make-instance 'command-result :redraw-board nil :message (is-four (slot-value context 'board) x y))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'set-level
			 :infoFn (lambda () "set-level <n>: Set the maximum traversal depth")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-level) context))
			 :execFn (lambda (context level)
				   (setf (slot-value context 'difficulty-level) level)
				   (make-instance 'command-result :redraw-board t :message nil)
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'play
			 :infoFn (lambda () "play <column>: Play a move and get computers counter move")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-x) context))
			 :execFn (lambda (context x)
				   (let ((y nil) (board nil) (computers-color nil) (counter-move nil))
				     (setf y (find-row (slot-value context 'board) x))
				     (if (not y)
					 (make-instance 'command-result :redraw-board nil :message "Invalid move. No place left in given column")
				       (progn
					 (setf board (set-field (slot-value context 'board) x y (slot-value context 'players-color)))
					 (setf (slot-value context 'board) board)
					 (if (is-four board x y)
					     (make-instance 'command-result :redraw-board t :message
							    (funcall (slot-value context 'format-alert-message) "YOU ARE THE WINNER"))
					   (progn
					     (setf computers-color (invert-color (slot-value context 'players-color)))
					     (setf counter-move (best-move board computers-color (slot-value context 'difficulty-level)))
					     (if (not counter-move)
						 (make-instance 'command-result :redraw-board true :message "No counter move found")
					       (progn
						 (setf board (set-field board (first counter-move) (second counter-move) computers-color))
						 (setf (slot-value context 'board) board)
						 (if (is-four board (first counter-move) (second counter-move))
						     (make-instance 'command-result :redraw-board t :message
								    (format nil "Computers move is ~a~%~a" (first counter-move)
									    (funcall (slot-value context 'format-alert-message) "THE COMPUTER HAS WON")))
						   (make-instance 'command-result :redraw-board t :message
								  (format nil "Computers move is ~a" (first counter-move)))
						   )
						 )
					       )
					     )
					   )
					 )
				       )
				     )
				   )
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
(defun format-context (context)
  (let ((formatter (funcall (slot-value context 'board-formatter-factory))))
    (format-board (slot-value context 'board) formatter)
    (format t "~%Level: ~a Your color: ~a~%"
	    (slot-value context 'difficulty-level)
	    (format-cell-value formatter (slot-value context 'players-color)))
    ))

(defun read-cmd ()
  (read-from-string (concatenate 'string "(" (read-line) ")"))
  )

;;
;; Execute a command entered into the game repl
;;
(defun exec-command (opcode context args)
  (setf parsed-args (handler-case (funcall (slot-value opcode 'parseArgsFn) args context)
					 (invalid-arguments (err) err)
					 ))
    (if (listp parsed-args)
	(apply (slot-value opcode 'execFn) context parsed-args)
      (make-instance 'command-result :redraw-board nil :message (slot-value parsed-args 'text)))
    )

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
			 (princ "Enter command: ")
			 (setf cmd (read-cmd))
			 (princ #\newline)
			 (cond
			  ((equal (car cmd) '()) (print-help-text command-table) (setf result 'continue))
			  ((equal (car cmd) 'q) (format t "Bye.~%Enter (ext:quit) to exit clisp~%") (setf result nil))
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
			       (format-context context))
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

(defun create-bw-board-formatter ()
  (make-instance 'cell-formats))

(defun create-colorful-board-formatter ()
  (make-instance 'colorful-cell-formats))

(defun create-default-context ()
  (let ((context (make-instance 'context)))
    (setf (slot-value context 'board) (create-board *CLASSIC-WIDTH* *CLASSIC-HEIGHT*))
    (setf (slot-value context 'players-color) 'W)
    (setf (slot-value context 'board-formatter-factory) #'create-colorful-board-formatter)
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

