
#|

 #####                                           #       
#     #  ####  #    # #    # ######  ####  ##### #    #  
#       #    # ##   # ##   # #      #    #   #   #    #  
#       #    # # #  # # #  # #####  #        #   #    #  
#       #    # #  # # #  # # #      #        #   ####### 
#     # #    # #   ## #   ## #      #    #   #        #  
 #####   ####  #    # #    # ######  ####    #        #  
                                                         
|#



(load "tools.lisp")
(load "field.lisp")
(load "context.lisp")
(load "command.lisp")
(load "commandresult.lisp")
(load "classic.lisp")


(defun parse-color (c)
  (if (equal c 'W) *WHITE* (if (equal c 'B) *BLACK* (progn (princ "Invalid color") (princ #\newline) nil)))
  )

;; Developer command table
(defun create-command-table ()
  (let ((table ()))

    (push (list 'board (make-instance 'command
				      :infoFn (lambda () "board")
				      :descriptionFn (lambda () "Prints the current board")
				      :execFn (lambda (context args)
						(make-instance 'command-result :redraw-board t :message nil)
						)
				      )) table)

    (push (list 'is-field-set (make-instance 'command
				      :infoFn (lambda () "is-field-set x y")
				      :descriptionFn (lambda () "Checks if a piece has been put into the given position")
				      :execFn (lambda (context args)
						(make-instance 'command-result :redraw-board nil :message (is-field-set (slot-value context 'board) (first args) (second args)))
						)
				      )) table)

    (push (list 'board-score (make-instance 'command
				      :infoFn (lambda () "board-score x y")
				      :descriptionFn (lambda () "Calculates the field score according to the given position. The position represents the latest move.")
				      :execFn (lambda (context args)
						(make-instance 'command-result :redraw-board nil :message (board-score (slot-value context 'board) (first args) (second args)))
						)
				      )) table)

    (push (list 'max-line-length-at (make-instance 'command
				      :infoFn (lambda () "max-line-length-at x y")
				      :descriptionFn (lambda () "Returns the maximum line length at the given position")
				      :execFn (lambda (context args)
						(make-instance 'command-result :redraw-board nil :message (max-line-length-at (slot-value context 'board) (first args) (second args)))
						)
				      )) table)

    (push (list 'put (make-instance 'command
				    :infoFn (lambda () "put color x y")
				    :descriptionFn (lambda () "Puts a piece into the board at the given position. Colors are B and W")
				    :execFn (lambda (context args)
					      (let ((board (slot-value context 'board)) (color (parse-color (first args))))
						(if color
						    (progn 
						      (setf board (set-field board (second args) (third args) color))
						      (setf (slot-value context 'board) board)
						      (make-instance 'command-result :redraw-board t :message nil))
						  (make-instance 'command-result :redraw-board nil :message nil)
						)))
				    )) table)

    (push (list 'best-move (make-instance 'command
				    :infoFn (lambda () "best-move color")
				    :descriptionFn (lambda () "Calculates the next best move for given color. Colors are B and W")
				    :execFn (lambda (context args)
					      (let ((board (slot-value context 'board)) (color (parse-color (first args))))
						(if color
						    (progn
						      (let ((result (best-move board color)))
							;;(print result)
							;;(setf board (set-field board (first result) (second result) color))
							;;(setf (slot-value context 'board) board)
							(make-instance 'command-result :redraw-board t :message result)))
						  (make-instance 'command-result :redraw-board nil :message nil)
						)))
				    )) table)

    
    (push (list 'is-white (make-instance 'command
					 :infoFn (lambda () "is-white x y")
					 :descriptionFn (lambda () "Checks if the piece at the given position is of color white")
					 :execFn (lambda (context args)
						   (make-instance 'command-result :redraw-board nil :message (is-field-color-p (slot-value context 'board) (first args) (second args) *WHITE*))
						   )
					 )) table)

    (push (list 'is-black (make-instance 'command
					 :infoFn (lambda () "is-black x y")
					 :descriptionFn (lambda () "Checks if the piece at the given position is of color black")
					 :execFn (lambda (context args)
						   (make-instance 'command-result :redraw-board nil :message (is-field-color-p (slot-value context 'board) (first args) (second args) *BLACK*))
						   )
					 )) table)

    (push (list 'is-four (make-instance 'command
					:infoFn (lambda () "is-four x y")
					:descriptionFn (lambda () "Checks if at the given position four pieces are in a line")
					:execFn (lambda (context args)
						  (make-instance 'command-result :redraw-board nil :message (is-four (slot-value context 'board) (first args) (second args)))
						  )
					)) table)


    
    table
    ))


(defun print-help-text (command-table &optional more)
  (princ "Available commands are")
  (princ #\newline)
  (dolist (cmd command-table)
    (princ (funcall (slot-value (car (cdr cmd)) 'infoFn)))
    (princ #\newline)
    (if more (progn
	       (princ #\tab)
	       (princ (funcall (slot-value (car (cdr cmd)) 'descriptionFn)))
	       (princ #\newline)
	       ))
    ))


(defun read-cmd ()
  (read-from-string (concatenate 'string "(" (read-line) ")"))
  )

(defun cmd-loop (command-table)
  (let ((board (create-board)) (cmd nil) (opcode nil) (result nil) (context (make-instance 'context)))
    ;; Body of let
    (setf (slot-value context 'board) (create-board))
    (print (slot-value context 'board))
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
			   (setf opcode (assoc (car cmd) command-table :test #'equal))
			   ;; execute implementation
			   (if opcode
			       (progn
				 (setf result (funcall (slot-value (car (cdr opcode)) 'execFn) context (cdr cmd)))
				 (if (slot-value result 'redraw-board) (progn (print (slot-value context 'board)) (princ #\newline)))
				 (princ (slot-value result 'message))
				 (princ #\newline)
				 )
			     (setf result 'continue))
			   ))
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

