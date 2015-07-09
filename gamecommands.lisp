
;;
;; Implementations of the game commands that are executed via the game repl
;;
;; TODO: load required dependencies
;;


(defun game-command-board (context)
  (declare (ignore context))
  (make-instance 'command-result :redraw-board t :message nil)
  )

(defun game-command-put (context color x y)
  (let ((board (slot-value context 'board)))
    (setf board (set-field board x y color))
    (setf (slot-value context 'board) board)
    (make-instance 'command-result :redraw-board t :message nil)
    ))

(defun game-command-set-board-size (context width height)
  (let ((new-board (create-board width height)))
    (setf (slot-value context 'board) new-board)
    (make-instance 'command-result :redraw-board t :message nil)
    ))

(defun game-command-hint (context color)
  (let ((board (slot-value context 'board)))
    (let ((result (best-move board color (slot-value context 'difficulty-level))))
      (make-instance 'command-result :redraw-board t :message result))
  ))

(defun game-command-is-four (context x y)
  (make-instance 'command-result :redraw-board nil :message (is-four (slot-value context 'board) x y))
  )

(defun game-command-set-level (context level)
  (setf (slot-value context 'difficulty-level) level)
  (make-instance 'command-result :redraw-board t :message nil)
  )

(defun game-command-continue (context)
  (let ((board (slot-value context 'board)) (computers-color nil) (counter-move nil) (highlight-cells nil))
    (setf computers-color (invert-color (slot-value context 'players-color)))
    (setf counter-move (best-move board computers-color (slot-value context 'difficulty-level)))
    (if (not counter-move)
	(make-instance 'command-result :redraw-board t :message "No counter move found")
      (progn
	(setf highlight-cells (list (list (first counter-move) (second counter-move))))
	(setf board (set-field board (first counter-move) (second counter-move) computers-color))
	(setf (slot-value context 'board) board)
	(if (is-four board (first counter-move) (second counter-move))
	    (make-instance 'command-result
			   :redraw-board t
			   :highlight-cells (max-line-at board (first counter-move) (second counter-move) computers-color)
			   :message (format nil "Computers move is ~a~%~a" (first counter-move)
					    (funcall (slot-value context 'format-alert-message) "THE COMPUTER HAS WON")))
	  (make-instance 'command-result
			 :redraw-board t
			 :highlight-cells highlight-cells
			 :message (format nil "Computers move is ~a" (first counter-move)))
	  )
	)
      )))

(defun game-command-play (context x)
  (let ((y nil) (board nil) )
    (setf y (find-row (slot-value context 'board) x))
    (if (not y)
	(make-instance 'command-result :redraw-board nil :message "Invalid move. No place left in given column")
      (progn
	(setf board (set-field (slot-value context 'board) x y (slot-value context 'players-color)))
	(setf (slot-value context 'board) board)
	(if (is-four board x y)
	    (make-instance 'command-result
			   :redraw-board t
			   :message (funcall (slot-value context 'format-alert-message) "YOU ARE THE WINNER")
			   :highlight-cells (max-line-at board x y (slot-value context 'players-color))
			   )
	  ;; continue with calculation of computers answer
	  (game-command-continue context)
	  )))))

(defun game-command-toggle-color (context)
    (setf (slot-value context 'players-color) (invert-color (slot-value context 'players-color)))
    (make-instance 'command-result :redraw-board t :message "Toggled players color")
    )
	
