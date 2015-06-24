
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

(defun format-player-has-won()
  (format nil "You have won")
  )

(define-condition invalid-arguments (error)
  ((text :initarg :text :reader text)))

(defun find-element (list equalFn)
  (let ((elem (first list)))
    (if (or (not elem) (funcall equalFn elem)) elem (find-element (cdr list) equalFn))
    ))

(defun parse-x (x)
  (cond 
	 ((not (integerp x)) (error 'invalid-arguments :text "Not a number"))
	 ((> x (+ *WIDTH* -1)) (error 'invalid-arguments :text (format nil "Column number too large. Allowed values are ~a...~a" 0 (+ *WIDTH* -1))))
	 ((< x 0) (error 'invalid-arguments :text (format nil "Column number too small. Allowed values are ~a...~a" 0 (+ *WIDTH* -1))))
	 (t x)
	 )
  )

(defun parse-y (y)
  (cond 
	 ((not (integerp y)) (error 'invalid-arguments :text "Not a number"))
	 ((> y (+ *HEIGHT* -1)) (error 'invalid-arguments :text (format nil "Row number too large. Allowed values are ~a...~a" 0 (+ *HEIGHT* -1))))
	 ((< y 0) (error 'invalid-arguments :text (format nil "Row number too small. Allowed values are ~a...~a" 0 (+ *HEIGHT* -1))))
	 (t y)
	 )
  )

(defun parse-level (level)
  (cond 
	 ((not (integerp level)) (error 'invalid-arguments :text "Not a number"))
	 ((< level 0) (error 'invalid-arguments :text (format nil "Level value too small. Allowed values are 0...n")))  
	 (t level)
	 )
  )

;; Transform color symbol into attached one
;; Todo: Study why symbols parsed by read are not attached to the context
;; http://www.flownet.com/gat/packages.pdf
(defun parse-color (c)
  (if (equal c 'W) *WHITE*
    (if (equal c 'B) *BLACK*
      (error 'invalid-arguments :text "Invalid color. Valid colors are W and B")
      )))

;; Developer command table
(defun create-command-table ()
  (let ((table ()))

    
    (push (make-instance 'command
			 :name 'board
			 :infoFn (lambda () "board")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args) (parse-arguments args '()))
			 :descriptionFn (lambda () "Prints the current board")
			 :execFn (lambda (context)
				   (make-instance 'command-result :redraw-board t :message nil)
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'is-field-set
			 :infoFn (lambda () "is-field-set x y")
			 :tags (list "DEVELOPER")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-x #'parse-y)))
			 :descriptionFn (lambda () "Checks if a piece has been put into the given position")
			 :execFn (lambda (context x y)
				   (make-instance 'command-result :redraw-board nil :message (is-field-set (slot-value context 'board) x y))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'board-score
			 :infoFn (lambda () "board-score x y")
			 :tags (list "DEVELOPER")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-x #'parse-y)))
			 :descriptionFn (lambda () "Calculates the field score according to the given position. This position represents the latest move.")
			 :execFn (lambda (context x y)
				   (make-instance 'command-result :redraw-board nil :message (board-score (slot-value context 'board) x y))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'max-line-length-at
			 :infoFn (lambda () "max-line-length-at x y")
			 :tags (list "DEVELOPER")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-x #'parse-y)))
			 :descriptionFn (lambda () "Returns the maximum line length at the given position")
			 :execFn (lambda (context x y)
				   (make-instance 'command-result :redraw-board nil :message (max-line-length-at (slot-value context 'board) x y))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'put
			 :infoFn (lambda () "put color x y")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-color #'parse-x #'parse-y)))
			 :tags (list "DEVELOPER")
			 :descriptionFn (lambda () "Puts a piece into the board at the given position. Colors are B and W")
			 :execFn (lambda (context color x y)
				   (let ((board (slot-value context 'board)))
				     (setf board (set-field board x y color))
				     (setf (slot-value context 'board) board)
				     (make-instance 'command-result :redraw-board t :message nil)
				     )
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'best-move
			 :infoFn (lambda () "best-move color")
			 :tags (list "DEVELOPER")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-color)))
			 :descriptionFn (lambda () "Calculates the next best move for given color. Colors are B and W")
			 :execFn (lambda (context color)
				   (let ((board (slot-value context 'board)))
				     (let ((result (best-move board color (slot-value context 'difficulty-level))))
				       (make-instance 'command-result :redraw-board t :message result)))
				   )
			 ) table)

    
    (push (make-instance 'command
			 :name 'is-white
			 :infoFn (lambda () "is-white x y")
			 :tags (list "DEVELOPER")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-x #'parse-y)))
			 :descriptionFn (lambda () "Checks if the piece at the given position is of color white")
			 :execFn (lambda (context x y)
				   (make-instance 'command-result :redraw-board nil :message (is-field-color-p (slot-value context 'board) x y *WHITE*))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'is-black
			 :infoFn (lambda () "is-black x y")
			 :tags (list "DEVELOPER")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-x #'parse-y)))
			 :descriptionFn (lambda () "Checks if the piece at the given position is of color black")
			 :execFn (lambda (context x y)
				   (make-instance 'command-result :redraw-board nil :message (is-field-color-p (slot-value context 'board) x y *BLACK*))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'is-four
			 :infoFn (lambda () "is-four x y")
			 :tags (list "DEVELOPER")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-x #'parse-y)))
			 :descriptionFn (lambda () "Checks if at the given position four pieces are in a line")
			 :execFn (lambda (context x y)
				   (make-instance 'command-result :redraw-board nil :message (is-four (slot-value context 'board) x y))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'set-level
			 :infoFn (lambda () "set-level n")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-level)))
			 :descriptionFn (lambda () "Sets the skill level of the computer (maximum traversal depth). Allowed value are 0..n")
			 :execFn (lambda (context level)
				   (setf (slot-value context 'difficulty-level) level)
				   (make-instance 'command-result :redraw-board t :message nil)
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'play
			 :infoFn (lambda () "play column-number")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-x)))
			 :descriptionFn (lambda () "Do a move and get computers counter move")
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


(defun print-help-text (command-table &optional more)
  (format t "Available commands are:~%")
  (dolist (cmd command-table)
    (princ (funcall (slot-value cmd 'infoFn)))
    (princ #\newline)
    (if more (progn
	       (princ #\tab)
	       (princ (funcall (slot-value cmd 'descriptionFn)))
	       (princ #\newline)
	       ))
    )
  (if (not more) (format t "--? for more help~%"))
  )

(defun format-context (context)
  (format-board (slot-value context 'board) (funcall (slot-value context 'board-formatter-factory)))
  (princ #\newline)
  (format t "Difficulty level is ~a~%" (slot-value context 'difficulty-level))
  (format t "Your color is ~a~%" (slot-value context 'players-color))
  )

;; Todo: Reader should read first parameter as string and not as symbol
(defun read-cmd ()
  (read-from-string (concatenate 'string "(" (read-line) ")"))
  )

(defun exec-command (opcode context args)
  (setf parsed-args (handler-case (funcall (slot-value opcode 'parseArgsFn) args)
					 (invalid-arguments (err) err)
					 ))
    (if (listp parsed-args)
	(apply (slot-value opcode 'execFn) context parsed-args)
      (make-instance 'command-result :redraw-board nil :message (slot-value parsed-args 'text)))
    )



(defun cmd-loop (context-factory)
  (let ((context (funcall context-factory)))
    (let ( (cmd nil) (opcode nil) (result nil) (command-table (slot-value context 'command-table)))
      (format-context context)
      (princ #\newline)
      (flet ((do-command ()
			 (princ "Enter command (q to quit, r to restart, ? for help): ")
			 ;; read command
			 (setf cmd (read-cmd))
			 (cond
			  ((equal (car cmd) '?) (print-help-text command-table) (setf result 'continue))
			  ((equal (car cmd) '--?) (print-help-text command-table t) (setf result 'continue))
			  ((equal (car cmd) 'q) (format t "Bye.~%Enter (ext:quit) to exit clisp~%") (setf result nil))
			  ((equal (car cmd) 'r)
			   (setf context (funcall context-factory))
			   (format-context context)
			   (setf result 'continue)) 
			  (t 
			   ;; get implementation of command
			   (setf opcode (find-element command-table (lambda (command) (equal (car cmd)  (slot-value command 'name)))))
			   ;; execute implementation
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
    (setf (slot-value context 'board) (create-board))
    (setf (slot-value context 'players-color) 'W)
    (setf (slot-value context 'board-formatter-factory) #'create-colorful-board-formatter)
    (setf (slot-value context 'command-table) (create-command-table))
    (setf (slot-value context 'difficulty-level) 4)
    (setf (slot-value context 'format-alert-message)
	  (lambda (msg)
	    (format nil "~c[32m~a~c[0m" #\Esc msg #\Esc))) 
    context
    ))


;; all commands
(defun lets-go()
  (cmd-loop (lambda ()
 	      (format t "~%~%Welcome to Connect4~%~%")
	      (create-default-context))))

;; player commands only
(defun lets-play()
  (cmd-loop (lambda ()
 	      (format t "~%~%Welcome to Connect4~%~%")
	      (let ((context (create-default-context)))
		(setf (slot-value context 'command-table)
		      (remove-if-not (lambda (cmd)
				       (find "PLAYER" (slot-value cmd 'tags) :test #'equal))
				     (slot-value context 'command-table)))
		context
		))))


#|
(defparameter *TESTBOARD* nil)

(defun test-it ()
  (setf *testboard* (create-board))
  (nset-field *testboard* 4 4 *WHITE*)
  (nset-field *testboard* 3 4 *WHITE*)
  (nset-field *testboard* 0 4 *BLACK*)
  (nset-field *testboard* 0 3 *BLACK*)
  (nset-field *testboard* 0 2 *WHITE*)
  (format-board *testboard* (make-instance 'colorful-cell-formats))
  (best-move *testboard* *BLACK*)  
  )

|#

  
