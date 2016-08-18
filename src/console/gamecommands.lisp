
(in-package :connect4-console)


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
  (let ((moves (movegenerator:generate-moves (slot-value context 'board))))
    (if (or (not moves) (= 0 (length moves)))
	(progn
	  (setf (slot-value context 'draws) (+ 1 (slot-value context 'draws)))
	  (format-message *message-formatter* "Draw! No more moves left.")
	  (setf (slot-value context 'state) GAME-STATE-FINAL)
	  ))))

;; TODO: Integrate into code. Remove second call to get-connected-pieces
(defun is-four (board x y)
  (>= (length (get-connected-pieces board x y)) 4))

(defun toggle-color (color)
  (if (eq color WHITE) BLACK WHITE))

(define-condition quit-game (error)
  ((text :initarg :text :reader text)))

(defun game-command-set-board-size (context width height)
  (setf (slot-value context 'board) (create-board width height))
  (setf (slot-value context 'state) GAME-STATE-CONTINUE)
  (format-context context)
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
  (setf (slot-value context 'players-color) (toggle-color (slot-value context 'players-color)))
  (format-context context)
  (setf (slot-value context 'state) GAME-STATE-CONTINUE)
  t)

(defun game-command-restart (context)
  (game-command-set-board-size context (get-width (slot-value context 'board)) (get-height (slot-value context 'board)))
  (format-context context)
  (format-message *message-formatter* "Restarted game")
  (setf (slot-value context 'state) GAME-STATE-CONTINUE)
  t)

(defun game-command-play-computer (context)
  (setf (slot-value context 'state) GAME-STATE-CONTINUE)
  (let ((computers-color (toggle-color (slot-value context 'players-color)))
	(counter-move nil) (counter-x nil) (counter-y nil) (counter-board nil))
    (setf counter-move (play (slot-value context 'board) computers-color (slot-value context 'difficulty-level)))
    (if (not (play-result-is-move counter-move))
	(progn
	  (setf (slot-value context 'draws) (+ 1 (slot-value context 'draws)))
	  (format-context context)
	  (format-message *message-formatter* "No counter move found")
	  (setf (slot-value context 'state) GAME-STATE-FINAL))  
	(progn
	  (setf counter-x (play-result-column counter-move))
	  (setf counter-y (play-result-row counter-move))
	  (setf counter-board (set-field (slot-value context 'board) counter-x counter-y computers-color))
	  (setf (slot-value context 'board) counter-board)
	  (if (is-four counter-board counter-x counter-y)
	      (progn
		(setf (slot-value context 'loses) (+ 1 (slot-value context 'loses)))
		(format-context context (get-connected-pieces counter-board counter-x counter-y))
		(format-message *message-formatter* (format nil "Computers move is ~a with a score of ~a" counter-x (play-result-score counter-move)))
		(format-message *message-formatter* "COMPUTER HAS WON")
		(setf (slot-value context 'state) GAME-STATE-FINAL)
		)
	      (progn
		(format-context context (list (list counter-x counter-y)))
		(format-message *message-formatter* (format nil "Computers move is ~a with a score of ~a" counter-x (play-result-score counter-move)))
		(check-if-move-is-available context)
		)
	      ))))
  t)

(defun game-command-play-human (context x)
  (setf (slot-value context 'state) GAME-STATE-CONTINUE)
  (let ((players-color (slot-value context 'players-color)) (y nil) (counter-board nil))
    (setf y (drop (slot-value context 'board) x))
    (if (not y)
	(format-message *message-formatter* "Invalid move. No place left in given column")
	(progn
	  (setf counter-board (set-field (slot-value context 'board) x y players-color))
	  (setf (slot-value context 'board) counter-board)
	  (if (is-four counter-board x y)
	      (progn
		(setf (slot-value context 'wins) (+ 1 (slot-value context 'wins)))
		(format-context context (get-connected-pieces counter-board x y))
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
