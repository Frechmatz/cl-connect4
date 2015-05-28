
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
(load "command.lisp")


(defclass context ()
  (
   (board)
   ))

(defclass command-result ()
  (
   (redraw-board :initarg :redraw-board)
   ))


#|
(defun cmd-set-field (board x y color)
  (if (eq color 'W) (setf board (set-field board x y *WHITE*)) (if (eq color 'B) (setf board (set-field board x y *BLACK*))))
  board
  )


     (cond
      ((eq (car cmd) 'quit) (princ "Bye."))
      ((eq (car cmd) 'put) (setf board (cmd-set-field board (second cmd) (third cmd) (fourth cmd))) (princ board) (princ #\newline) (cmd-loop board))
      ((eq (car cmd) 'is-white) (princ "is-white ") (princ (second cmd)) (princ " ") (princ (third cmd)) (princ ": ") (princ (is-field-color-p board (second cmd) (third cmd) *WHITE*)) (princ #\newline) (cmd-loop board))
      ((eq (car cmd) 'is-black) (princ "is-black ") (princ (second cmd)) (princ " ") (princ (third cmd)) (princ ": ") (princ (is-field-color-p board (second cmd) (third cmd) *BLACK*)) (princ #\newline) (cmd-loop board))
      ((eq (car cmd) 'is-four) (princ "is-four ") (princ (second cmd)) (princ " ") (princ (third cmd)) (princ ": ") (princ (is-four board (second cmd) (third cmd))) (princ #\newline) (cmd-loop board))
      ((eq (car cmd) 'b) (princ board) (princ #\newline) (cmd-loop board))
      (t (princ "Unknown command: ") (princ (car cmd)) (princ #\newline) (cmd-help)  (cmd-loop board))
     )))

|#


(defun create-command-table ()
  (let ((table ()))
    (push (list 'put (make-instance 'command
				     :infoFn (lambda () (princ "put x y <W | B>") (princ #\newline))
				     :execFn (lambda (context args)
					       (princ "Called Put")
					       (princ #\newline)
					       (make-instance 'command-result :redraw-board t)
					       )
				     )) table)

    (push (list 'is-white (make-instance 'command
				     :infoFn (lambda () (princ "is-white x y") (princ #\newline))
				     :execFn (lambda (context args)
					       (princ "Called IsWhite")
					       (princ #\newline)
					       (make-instance 'command-result :redraw-board nil)
					       )
				     )) table)

    (push (list 'is-black (make-instance 'command
				     :infoFn (lambda () (princ "is-black x y") (princ #\newline))
				     :execFn (lambda (context args)
					       (princ "Called IsBlack")
					       (princ #\newline)
					       (make-instance 'command-result :redraw-board nil)
					       )
				     )) table)

    (push (list 'is-four (make-instance 'command
				     :infoFn (lambda () (princ "is-four x y") (princ #\newline))
				     :execFn (lambda (context args)
					       (princ "Called IsFour")
					       (princ #\newline)
					       (make-instance 'command-result :redraw-board nil)
					       )
				     )) table)

    table
    ))


(defun print-help-text (command-table)
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
			((equal (car cmd) 'quit) (princ "Bye.") (princ #\newline) (setf result nil))
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

