
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


;; Developer command table
(defun create-command-table ()
  (let ((table ()))

    (push (list 'board (make-instance 'command
				      :infoFn (lambda ()
						(princ "board")
						(princ #\newline)
						(princ #\tab)
						(princ "Prints the current board")
						(princ #\newline)
						)
				      :execFn (lambda (context args)
						(make-instance 'command-result :redraw-board t)
						)
				      )) table)

    (push (list 'is-field-set (make-instance 'command
				      :infoFn (lambda ()
						(princ "is-field-set x y")
						(princ #\newline)
						(princ #\tab)
						(princ "Checks if a piece has been put into the given position")
						(princ #\newline)
						)
				      :execFn (lambda (context args)
						(princ (is-field-set (slot-value context 'board) (first args) (second args)))
						(princ #\newline)
						(make-instance 'command-result :redraw-board nil)
						)
				      )) table)

    (push (list 'board-score (make-instance 'command
				      :infoFn (lambda ()
						(princ "board-score x y")
						(princ #\newline)
						(princ #\tab)
						(princ "Calculates the field score according to the given position. The position represents the latest move.")
						(princ #\newline)
						)
				      :execFn (lambda (context args)
						(princ (board-score (slot-value context 'board) (first args) (second args)))
						(princ #\newline)
						(make-instance 'command-result :redraw-board nil)
						)
				      )) table)

    (push (list 'max-line-length-at (make-instance 'command
				      :infoFn (lambda ()
						(princ "max-line-length-at x y")
						(princ #\newline)
						(princ #\tab)
						(princ "Returns the maximum line length at the given position")
						(princ #\newline)
						)
				      :execFn (lambda (context args)
						(princ (max-line-length-at (slot-value context 'board) (first args) (second args)))
						(princ #\newline)
						(make-instance 'command-result :redraw-board nil)
						)
				      )) table)

    (push (list 'put (make-instance 'command
				    :infoFn (lambda ()
					      (princ "put color x y")
					      (princ #\newline)
					      (princ #\tab)
					      (princ "Puts a piece into the board at the given position. Colors are B and W")
					      (princ #\newline)
					      )
				    :execFn (lambda (context args)
					      (let ((board (slot-value context 'board)) (color nil))
						(if (equal (first args) 'W)
						    (setf color *WHITE*)
						  (if (equal (first args) 'B) (setf color *BLACK*)))
						(if (not color)
						    (progn (princ "Invalid color") (princ #\newline))
						  (progn 
						    (setf board (set-field board (second args) (third args) color))
						    (setf (slot-value context 'board) board)))
						(make-instance 'command-result :redraw-board t)
						))
				    )) table)

    (push (list 'is-white (make-instance 'command
					 :infoFn (lambda ()
						   (princ "is-white x y")
						   (princ #\newline)
						   (princ #\tab)
						   (princ "Checks if the piece at the given position is of color white")
						   (princ #\newline)
						   )
					 :execFn (lambda (context args)
						   (princ (is-field-color-p (slot-value context 'board) (first args) (second args) *WHITE*))
						   (princ #\newline)
						   (make-instance 'command-result :redraw-board nil)
						   )
					 )) table)

    (push (list 'is-black (make-instance 'command
					 :infoFn (lambda ()
						   (princ "is-black x y")
						   (princ #\newline)
						   (princ #\tab)
						   (princ "Checks if the piece at the given position is of color black")
						   (princ #\newline)

						   )
					 :execFn (lambda (context args)
						   (princ (is-field-color-p (slot-value context 'board) (first args) (second args) *BLACK*))
						   (princ #\newline)
						   (make-instance 'command-result :redraw-board nil)
						   )
					 )) table)

    (push (list 'is-four (make-instance 'command
					:infoFn (lambda ()
						  (princ "is-four x y")
						  (princ #\newline)
						  (princ #\tab)
						  (princ "Checks if at the given position four pieces are in a line")
						  (princ #\newline)
						  )
					:execFn (lambda (context args)
						  (princ (is-four (slot-value context 'board) (first args) (second args)))
						  (princ #\newline)
						  (make-instance 'command-result :redraw-board nil)
						  )
					)) table)

    table
    ))


(defun print-help-text (command-table)
  (princ "Available commands are")
  (princ #\newline)
  (dolist (cmd command-table)
    (funcall (slot-value (car (cdr cmd)) 'infoFn))
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
		       (princ "Enter command (quit to quit, ? for help): ")
		       ;; read command
		       (setf cmd (read-cmd))
		       (cond
			((equal (car cmd) '?) (print-help-text command-table) (setf result 'continue))
			((equal (car cmd) 'quit) (princ "Bye.") (princ #\newline) (princ "Enter (ext:quit) to exit Lisp") (princ #\newline) (setf result nil))
			(t 
			   ;; get implementation of command
			   (setf opcode (assoc (car cmd) command-table :test #'equal))
			   ;; execute implementation
			   (if opcode
			       (progn
				 (setf result (funcall (slot-value (car (cdr opcode)) 'execFn) context (cdr cmd)))
				 (if (slot-value result 'redraw-board) (progn (print (slot-value context 'board)) (princ #\newline)))
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

