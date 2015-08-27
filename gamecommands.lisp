
(in-package :connect4)


;;;;
;;;; Game commands
;;;;
;;;; - may modify the game context
;;;; - may print context statuses or messages
;;;;   to stdout
;;;; - may return NIL to signal that the
;;;;   current command loop should be left.
;;;;   Used for exiting a nest command loop.
;;;;   Commands typically return t
;;;; - may indicate a final state by
;;;;   setting the game-state property of the
;;;;   game context to GAME-STATE-FINAL
;;;;

(defun check-if-move-is-available (context)
  "Helper function to detect a final state"
  (if (not (is-move-available (slot-value context 'board)))
      (progn
	(setf (slot-value context 'draws) (+ 1 (slot-value context 'draws)))
	(format-message *message-formatter* "Draw! No more moves left.")
	(setf (slot-value context 'state) GAME-STATE-FINAL)
	)))


(define-condition quit-game (error)
  ((text :initarg :text :reader text)))

(defun game-command-set-board-size (context width height)
  (setf (slot-value context 'board) (create-board width height))
  (setf (slot-value context 'state) GAME-STATE-CONTINUE)
  (format-context context)
  t)

(defun game-command-hint (context)
  (let ((result (minmax (slot-value context 'board) (slot-value context 'players-color) (slot-value context 'difficulty-level))))
    (format-message *message-formatter* (format nil "Recommended move is column ~a with a score of ~a" (first result) (third result))))
  (setf (slot-value context 'state) GAME-STATE-CONTINUE)
  t)

(defun game-command-print-board (context)
  (format-context context)
  (setf (slot-value context 'state) GAME-STATE-CONTINUE)
  t)

(defun game-command-set-level (context level)
  (setf (slot-value context 'difficulty-level) level)
  (format-context context)
  (setf (slot-value context 'state) GAME-STATE-CONTINUE)
  t)

(defun game-command-toggle-color (context)
  (setf (slot-value context 'players-color) (invert-color (slot-value context 'players-color)))
  (format-context context)
  (setf (slot-value context 'state) GAME-STATE-CONTINUE)
  t)

(defun game-command-restart (context)
  (game-command-set-board-size context (get-board-width (slot-value context 'board)) (get-board-height (slot-value context 'board)))
  (format-context context)
  (format-message *message-formatter* "Restarted game")
  (setf (slot-value context 'state) GAME-STATE-CONTINUE)
  t)

(defun game-command-play-computer (context)
  (setf (slot-value context 'state) GAME-STATE-CONTINUE)
  (let ((computers-color (invert-color (slot-value context 'players-color)))
	(counter-move nil) (counter-x nil) (counter-y nil) (counter-board nil))
    (setf counter-move (minmax (slot-value context 'board) computers-color (slot-value context 'difficulty-level)))
    (if (not counter-move)
	(progn
	  (setf (slot-value context 'draws) (+ 1 (slot-value context 'draws)))
	  (format-context context)
	  (format-message *message-formatter* "No counter move found")
	  (setf (slot-value context 'state) GAME-STATE-FINAL))  
	(progn
	  (setf counter-x (first counter-move))
	  (setf counter-y (second counter-move))
	  (setf counter-board (set-field (slot-value context 'board) counter-x counter-y computers-color))
	  (setf (slot-value context 'board) counter-board)
	  (if (is-four counter-board counter-x counter-y)
	      (progn
		(setf (slot-value context 'loses) (+ 1 (slot-value context 'loses)))
		(format-context context (max-line-at counter-board counter-x counter-y computers-color))
		(format-message *message-formatter* (format nil "Computers move is ~a with a score of ~a" counter-x (third counter-move)))
		(format-message *message-formatter* "COMPUTER HAS WON")
		(setf (slot-value context 'state) GAME-STATE-FINAL)
		)
	      (progn
		(format-context context (list (list counter-x counter-y)))
		(format-message *message-formatter* (format nil "Computers move is ~a with a score of ~a" counter-x (third counter-move)))
		(check-if-move-is-available context)
		)
	      ))))
  t)

(defun game-command-play-human (context x)
  (setf (slot-value context 'state) GAME-STATE-CONTINUE)
  (let ((players-color (slot-value context 'players-color)) (y nil) (counter-board nil))
    (setf y (find-row (slot-value context 'board) x))
    (if (not y)
	(format-message *message-formatter* "Invalid move. No place left in given column")
	(progn
	  (setf counter-board (set-field (slot-value context 'board) x y players-color))
	  (setf (slot-value context 'board) counter-board)
	  (if (is-four counter-board x y)
	      (progn
		(setf (slot-value context 'wins) (+ 1 (slot-value context 'wins)))
		(format-context context (max-line-at counter-board x y players-color))
		(format-message *message-formatter* "YOU ARE THE WINNER")
		(setf (slot-value context 'state) GAME-STATE-FINAL)
		)
	      (game-command-play-computer context)
	      ))))
  t)

(defun game-command-quit (context)
  (declare (ignore context))
  ;; Signal quit. (todo: consider if this is a good idea)
  (error 'quit-game :text "Bye"))


(defun game-command-set-board (context creator-fn)
  (setf (slot-value context 'board) (funcall creator-fn))
  (format-context context)
  (format-message *message-formatter* "Board set")
  (setf (slot-value context 'state) GAME-STATE-CONTINUE)
  t)
