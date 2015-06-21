
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

;; Todo: Check min/max of value
(defun parse-x (x)
  (if (integerp x) x (error 'invalid-arguments :text "Not a number"))
  )

;; Todo: Check min/max of value
(defun parse-y (y)
  (if (integerp y) y (error 'invalid-arguments :text "Not a number"))
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
			 :parseArgsFn (lambda (args) (parse-arguments args '()))
			 :descriptionFn (lambda () "Prints the current board")
			 :execFn (lambda (context)
				   (make-instance 'command-result :redraw-board t :message nil)
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'is-field-set
			 :infoFn (lambda () "is-field-set x y")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-x #'parse-y)))
			 :descriptionFn (lambda () "Checks if a piece has been put into the given position")
			 :execFn (lambda (context x y)
				   (make-instance 'command-result :redraw-board nil :message (is-field-set (slot-value context 'board) x y))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'board-score
			 :infoFn (lambda () "board-score x y")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-x #'parse-y)))
			 :descriptionFn (lambda () "Calculates the field score according to the given position. The position represents the latest move.")
			 :execFn (lambda (context x y)
				   (make-instance 'command-result :redraw-board nil :message (board-score (slot-value context 'board) x y))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'max-line-length-at
			 :infoFn (lambda () "max-line-length-at x y")
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
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-color)))
			 :descriptionFn (lambda () "Calculates the next best move for given color. Colors are B and W")
			 :execFn (lambda (context color)
				   (let ((board (slot-value context 'board)))
				     (let ((result (best-move board color)))
				       (make-instance 'command-result :redraw-board t :message result)))
				   )
			 ) table)

    
    (push (make-instance 'command
			 :name 'is-white
			 :infoFn (lambda () "is-white x y")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-x #'parse-y)))
			 :descriptionFn (lambda () "Checks if the piece at the given position is of color white")
			 :execFn (lambda (context x y)
				   (make-instance 'command-result :redraw-board nil :message (is-field-color-p (slot-value context 'board) x y *WHITE*))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'is-black
			 :infoFn (lambda () "is-black x y")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-x #'parse-y)))
			 :descriptionFn (lambda () "Checks if the piece at the given position is of color black")
			 :execFn (lambda (context x y)
				   (make-instance 'command-result :redraw-board nil :message (is-field-color-p (slot-value context 'board) x y *BLACK*))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'is-four
			 :infoFn (lambda () "is-four x y")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-x #'parse-y)))
			 :descriptionFn (lambda () "Checks if at the given position four pieces are in a line")
			 :execFn (lambda (context x y)
				   (make-instance 'command-result :redraw-board nil :message (is-four (slot-value context 'board) x y))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'play
			 :infoFn (lambda () "play x")
			 :parseArgsFn (lambda (args) (parse-arguments args (list #'parse-x)))
			 :descriptionFn (lambda () "Do a move and get computers counter move")
			 :execFn (lambda (context x)
				   (setf y (find-row (slot-value context 'board) x))
				   (if (not y)
				       (make-instance 'command-result :redraw-board nil :message "Invalid move. No place in column left.")
				     (progn
				       (setf board (set-field (slot-value context 'board) x y (slot-value context 'players-color)))
				       (setf (slot-value context 'board) board)
				       (if (is-four board x y)
					   (make-instance 'command-result :redraw-board t :message "You have won")
					 (progn
					   (setf computers-color (invert-color (slot-value context 'players-color)))
					   (setf counter-move (best-move board computers-color))
					   (if (not counter-move)
					       (make-instance 'command-result :redraw-board true :message "No counter move found")
					     (progn
					       (setf board (set-field board (first counter-move) (second counter-move) computers-color))
					       (setf (slot-value context 'board) board)
					       (if (is-four board (first counter-move) (second counter-move))
						   (make-instance 'command-result :redraw-board t :message "Computer has won")
						 (make-instance 'command-result :redraw-board t :message "Ok! Please continue"))
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
  (princ "Available commands are")
  (princ #\newline)
  (dolist (cmd command-table)
    (princ (funcall (slot-value cmd 'infoFn)))
    (princ #\newline)
    (if more (progn
	       (princ #\tab)
	       (princ (funcall (slot-value cmd 'descriptionFn)))
	       (princ #\newline)
	       ))
    ))

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



(defun cmd-loop (command-table)
  (let ((board (create-board)) (cmd nil) (opcode nil)  (result nil) (context (make-instance 'context)))
    ;; Body of let
    (setf (slot-value context 'board) (create-board))
    (setf (slot-value context 'players-color) 'W)
    (format-board (slot-value context 'board) (make-instance 'colorful-cell-formats))
    (princ #\newline)
    (flet ((do-command ()
		       (princ "Enter command (quit to quit, ? for help, --? for more help): ")
		       ;; read command
		       (setf cmd (read-cmd))
		       (cond
			((equal (car cmd) '?) (print-help-text command-table) (setf result 'continue))
			((equal (car cmd) '--?) (print-help-text command-table t) (setf result 'continue))
			((equal (car cmd) 'quit) (princ "Bye.") (princ #\newline) (princ "Enter (ext:quit) to exit Lisp") (princ #\newline) (setf result nil))
			(t 
			 ;; get implementation of command
			 (setf opcode (find-element command-table (lambda (command) (equal (car cmd)  (slot-value command 'name)))))
			 ;; execute implementation
			 (if opcode
			     (setf result (exec-command opcode context (cdr cmd)))
			   (setf result (make-instance 'command-result :redraw-board nil :message nil))
			   )
			 (if (slot-value result 'redraw-board)
			     (progn (format-board (slot-value context 'board) (make-instance 'colorful-cell-formats)) (princ #\newline)))
			 (princ (slot-value result 'message))
			 (princ #\newline)
			 )
			)
		       result))
	  ;; Body of flet
	  (do ((ergebnis (do-command) (do-command)))
	      ((not ergebnis))
	      )
	  )
    )
  )	  

(defun lets-go()
  (princ "Welcome to Connect4")
  (princ #\newline)
  (cmd-loop (create-command-table))
  )

