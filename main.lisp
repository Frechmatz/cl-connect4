
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
(load "command.lisp")
(load "commandresult.lisp")
(load "classic.lisp")


(define-condition malformed-argument (error)
  ((text :initarg :text :reader text)))

(defun find-element (list equalFn)
  (let ((elem (first list)))
    (if (or (not elem) (funcall equalFn elem)) elem (find-element (cdr list) equalFn))
    ))


(defun parse-color (c)
  (if (equal c 'W) *WHITE* (if (equal c 'B) *BLACK* (progn (princ "Invalid color") (princ #\newline) nil)))
  )

(defun parse-color-exc (c)
  (if (equal c 'W) *WHITE*
    (if (equal c 'B) *BLACK*
      (error 'malformed-argument :text "ungueltige farbe")
      )))

(defun parse-through (x)
  x)

(defun parse-args (args parsers)
  (let ((result ()) (parser nil) (arg nil))
    (dotimes (i (length args))
      (setf parser (nth i parsers))
      (setf arg (nth i args))
      (if (not parser)
	  (push arg result)
	(push (funcall parser arg) result)
	))
    result
    ))


;; Developer command table
(defun create-command-table ()
  (let ((table ()))


    (push (make-instance 'command
			 :name 'put-test
			 :infoFn (lambda () "put color x y")
			 :parseArgsFn (lambda (args) (parse-args args (list #'parse-color-exc nil nil)))
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
			 ) table)

    
    (push (make-instance 'command
			 :name 'board
			 :infoFn (lambda () "board")
			 :parseArgsFn (lambda (args) args)
			 :descriptionFn (lambda () "Prints the current board")
			 :execFn (lambda (context args)
				   (make-instance 'command-result :redraw-board t :message nil)
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'is-field-set
			 :infoFn (lambda () "is-field-set x y")
			 :parseArgsFn (lambda (args) args)
			 :descriptionFn (lambda () "Checks if a piece has been put into the given position")
			 :execFn (lambda (context args)
				   (make-instance 'command-result :redraw-board nil :message (is-field-set (slot-value context 'board) (first args) (second args)))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'board-score
			 :infoFn (lambda () "board-score x y")
			 :parseArgsFn (lambda (args) args)
			 :descriptionFn (lambda () "Calculates the field score according to the given position. The position represents the latest move.")
			 :execFn (lambda (context args)
				   (make-instance 'command-result :redraw-board nil :message (board-score (slot-value context 'board) (first args) (second args)))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'max-line-length-at
			 :infoFn (lambda () "max-line-length-at x y")
			 :parseArgsFn (lambda (args) args)
			 :descriptionFn (lambda () "Returns the maximum line length at the given position")
			 :execFn (lambda (context args)
				   (make-instance 'command-result :redraw-board nil :message (max-line-length-at (slot-value context 'board) (first args) (second args)))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'put
			 :infoFn (lambda () "put color x y")
			 :parseArgsFn (lambda (args) args)
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
			 ) table)


    
    (push (make-instance 'command
			 :name 'best-move
			 :infoFn (lambda () "best-move color")
			 :parseArgsFn (lambda (args) args)
			 :descriptionFn (lambda () "Calculates the next best move for given color. Colors are B and W")
			 :execFn (lambda (context args)
				   (let ((board (slot-value context 'board)) (color (parse-color (first args))))
				     (if color
					 (progn
					   (let ((result (best-move board color)))
					     (make-instance 'command-result :redraw-board t :message result)))
				       (make-instance 'command-result :redraw-board nil :message nil)
				       )))
			 ) table)

    
    (push (make-instance 'command
			 :name 'is-white
			 :infoFn (lambda () "is-white x y")
			 :parseArgsFn (lambda (args) args)
			 :descriptionFn (lambda () "Checks if the piece at the given position is of color white")
			 :execFn (lambda (context args)
				   (make-instance 'command-result :redraw-board nil :message (is-field-color-p (slot-value context 'board) (first args) (second args) *WHITE*))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'is-black
			 :infoFn (lambda () "is-black x y")
			 :parseArgsFn (lambda (args) args)
			 :descriptionFn (lambda () "Checks if the piece at the given position is of color black")
			 :execFn (lambda (context args)
				   (make-instance 'command-result :redraw-board nil :message (is-field-color-p (slot-value context 'board) (first args) (second args) *BLACK*))
				   )
			 ) table)

    (push (make-instance 'command
			 :name 'is-four
			 :infoFn (lambda () "is-four x y")
			 :parseArgsFn (lambda (args) args)
			 :descriptionFn (lambda () "Checks if at the given position four pieces are in a line")
			 :execFn (lambda (context args)
				   (make-instance 'command-result :redraw-board nil :message (is-four (slot-value context 'board) (first args) (second args)))
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

(defun cmd-loop (command-table)
  (let ((board (create-board)) (cmd nil) (opcode nil) (args nil) (result nil) (context (make-instance 'context)))
    ;; Body of let
    (setf (slot-value context 'board) (create-board))
    (setf (slot-value context 'players-color) 'W)
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
			 (setf opcode (find-element command-table (lambda (command) (equal (car cmd)  (slot-value command 'name)))))
			 ;; execute implementation
			   (if opcode
			       (progn
				 ;;(setf args (cdr cmd))
				 ;;
				 (setf result (funcall (slot-value opcode 'execFn) context (cdr cmd)))
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

