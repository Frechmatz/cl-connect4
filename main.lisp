
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
			 :execFn (lambda (cb context)
				   (declare (ignore context))
				   (funcall cb nil t nil nil))
			 ) table)

    (push (make-instance 'command
			 :name 'put
			 :infoFn (lambda () "put color <column> <row>: Put a piece into the board. column and row can be entered in hex")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-color #'parse-x #'parse-y) context))
			 :tags (list "DEVELOPER")
			 :execFn (lambda (cb context color x y)
				   (funcall cb (setf (slot-value context 'board) (game-command-put (slot-value context 'board) color x y)) t nil nil)
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'set-board-size
			 :infoFn (lambda () "set-board-size <width> <height>: Set size of the board")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-board-dimension #'parse-board-dimension) context))
			 :tags (list "DEVELOPER" "PLAYER")
			 :execFn (lambda (cb context width height)
				   (declare (ignore context))
				   (funcall cb (create-board width height) t nil nil))
			 ) table)

    (push (make-instance 'command
			 :name 'hint
			 :infoFn (lambda () "hint: Show next move the computer would do")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args context) (parse-arguments args '() context))
			 :execFn (lambda (cb context)
				   (let ((result (best-move (slot-value context 'board) (slot-value context 'players-color) (slot-value context 'difficulty-level))))
				     (funcall cb nil nil
					      (format nil "Recommended move is column ~a with a score of ~a" (first result) (third result))
					      nil)))
			 ) table)

    (push (make-instance 'command
			 :name 'set-level
			 :infoFn (lambda () "set-level <n>: Set the maximum traversal depth")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-level) context))
			 :execFn (lambda (cb context level)
				   (setf (slot-value context 'difficulty-level) level)
				   (funcall cb nil t nil nil))
			 ) table)

    (push (make-instance 'command
			 :name 'play
			 :infoFn (lambda () "play <column>: Play a move and get computers counter move. column can be entered in hex")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args context) (parse-arguments args (list #'parse-x) context))
			 :execFn (lambda (cb context x)
				   (let ((players-color (slot-value context 'players-color))
					 (computers-color (invert-color (slot-value context 'players-color)))
					 (difficulty-level (slot-value context 'difficulty-level))

					 )
				     (game-command-throw-piece
				      (slot-value context 'board) players-color x
				      (lambda (x y board)
					(if (not board)
					    (funcall cb nil nil "Invalid move. No place left in given column" nil)
					    (if (is-four board x y)
						(funcall cb board nil "YOU ARE THE WINNER" (max-line-at board x y players-color))
						(game-command-play
						 board computers-color difficulty-level
						 (lambda (counter-x counter-y counter-score counter-board)
						   (declare (ignore counter-score))
						   (if (not counter-board)
						       (funcall cb board nil "No counter move found" (list (list x y)))
						       (if (is-four counter-board counter-x counter-y)
							   (funcall cb counter-board t "COMPUTER HAS WON"
								    (max-line-at counter-board counter-x counter-y computers-color))
							   (funcall cb counter-board t (format nil "Computers move is ~a" counter-x) (list (list counter-x counter-y)))
							   ))))))))))
			 ) table)


    (push (make-instance 'command
			 :name 'toggle-color
			 :infoFn (lambda () "toggle-color: Toggle the players color")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args context) (parse-arguments args '() context))
			 ;; todo anpassen
			 :execFn (lambda (cb context)
				   (setf (slot-value context 'players-color) (invert-color (slot-value context 'players-color)))
				   (funcall cb nil t nil nil))
			 ) table)

    table
    ))

#|
(defun final-state-reached-command-table ()
  (let ((table ()))

    (push (make-instance 'command
			 :name 'r
			 :infoFn (lambda () "Enter r to start a new game")
			 :tags (list "DEVELOPER" "PLAYER")
			 :parseArgsFn (lambda (args context) (parse-arguments args '() context))
			 :execFn #'game-command-board
			 ) table)

    table
    ))
|#




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
;; cb => f( 
;;
(defun exec-command (opcode context args)
  (let ((parsed-args (handler-case (funcall (slot-value opcode 'parseArgsFn) args context)
					 (invalid-arguments (err) err)
					 )))
    (if (listp parsed-args)
	(apply (slot-value opcode 'execFn)
	       (lambda (new-board force-redraw-board message highlight-cells)
		 (if new-board
		     (progn
		       (setf (slot-value context 'board) new-board)
		       (make-instance 'command-result :redraw-board t :message message :highlight-cells highlight-cells)
		       )
		     (make-instance 'command-result :redraw-board force-redraw-board :message message :highlight-cells nil)))
	       context
	       parsed-args)
	(progn
	  (make-instance 'command-result :redraw-board nil :message (slot-value parsed-args 'text) :highlight-cells nil)
	))))


;;
;; ****************************
;; Main game repl
;; ****************************
;;
(defun cmd-loop (context-factory)
  (let ((context (funcall context-factory)))
    (let ( (cmd nil) (opcode nil) (result nil) (command-table (car (slot-value context 'command-table-stack))))
      (format-context context)
      (princ #\newline)
      (print-help-text command-table)
      (flet ((do-command ()
			 (format t "Enter command: ")
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

(defun create-default-context ( &key (colors-not-supported nil))
  (let ((context (make-instance 'context)))
    (setf (slot-value context 'board) (create-board *CLASSIC-WIDTH* *CLASSIC-HEIGHT*))
    (setf (slot-value context 'players-color) 'W)
    (if (not colors-not-supported)
	(setf (slot-value context 'cell-formatter) (make-instance 'colorful-cell-formatter))
	(setf (slot-value context 'cell-formatter) (make-instance 'cell-formatter))
	)
    (setf (slot-value context 'command-table-stack) (list (create-command-table)))
    (setf (slot-value context 'difficulty-level) 4)
    (setf (slot-value context 'format-alert-message)
	  (lambda (msg)
	    (if (not colors-not-supported)
		(format nil "~c[32m~a~c[0m" #\Esc msg #\Esc)
		(format nil "~a" msg)
		)))
    context
    ))

;;
;; ****************************
;; Start game in developer mode
;; ****************************
;;
(defun lets-go( &key (colors-not-supported nil))
  (cmd-loop (lambda ()
 	      (format t "~%~%Welcome to Connect4~%~%")
	      (create-default-context :colors-not-supported colors-not-supported))))

;;
;; *************************
;; Start game in player mode
;; *************************
;;
(defun lets-play( &key (colors-not-supported nil))
  (cmd-loop (lambda ()
 	      (format t "~%~%Welcome to Connect4~%~%")
	      (let ((context (create-default-context :colors-not-supported colors-not-supported)))
		(let ((cmds (car (slot-value context 'command-table-stack))))
		  ;; filter away developer commands
		  (setf cmds (remove-if-not (lambda (cmd)
					      (find "PLAYER" (slot-value cmd 'tags) :test #'equal))
					    cmds))
		  (setf (slot-value context 'command-table-stack) (list cmds))
		  )
		context
		))))

