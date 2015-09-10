;;;;
;;;; Engine of classic Connect4
;;;;

(in-package :connect4)


(defconstant CLASSIC-WIDTH 7 "Board width of the original game") 
(defconstant CLASSIC-HEIGHT 6 "Board height of the original game") 

(defvar *engine-configuration-skip-randomizer* nil
  "Set variable to true to disable that a random move will be chosen from all moves that have the same score.")
(defvar *engine-configuration-depth-relative-score* nil
  "Set variable to nil to disable that a score reflects the current traversal depth, e.g. at a current depth of 2, a score of 1.0 results in 0.1.")
(defvar *engine-configuration-score-calculation-considers-three* nil
  "Experimental. Set variable to t to enable that when there are three pieces in a row the resulting board-score will be increased.")
(defvar *engine-configuration-quit-row-evaluation-on-four* nil
  "Set to t to quit further evaluation of the current row if a winning situation has been detected.")

(defvar *engine-notification-reduced-scores*
  (lambda (board color is-opponent depth reduced-score all-scores)
  (declare (ignore board color is-opponent depth reduced-score all-scores))
  nil))


(defun print-engine-configuration ()
  (format t "Engine configuration:")
  (format t "~%*engine-configuration-skip-randomizer*: ~a" *engine-configuration-skip-randomizer*)
  (format t "~%*engine-configuration-depth-relative-score*: ~a" *engine-configuration-depth-relative-score*)
  (format t "~%*engine-configuration-score-calculation-considers-three*: ~a" *engine-configuration-score-calculation-considers-three*)
  (format t "~%*engine-configuration-quit-row-evaluation-on-four*: ~a" *engine-configuration-quit-row-evaluation-on-four*)
  (format t "~%")
  )


  
  

(defun board-score (board x y)
  "Evaluate the score of the board. x y: The current move. Returns a value 0 >= value <= 1, where 1 signals a winning position"
  (let ((l (max-line-length-at board x y (get-field board x y))))
    (if (>= l 4)
	1.0
	(if (not *engine-configuration-score-calculation-considers-three*)
	    0.0
	    (progn
	      ;; (princ "Considers three")
	      (if (>= l 3) 0.5 0.0))
	    )
    )))

(defun generate-moves (board)
  "Generate moves. Returns a list of (x y) coordinates of all possible moves"
  (let ( (moves ()) (row nil))
    (dotimes (x (get-board-width board))
      (setf row (find-row board x))
      (if row (push (list x row) moves))
      )
    moves
    ))

(defun is-move-available (board)
  "Check if a move is available for the given board"
  (let (( move-left nil))
    (dotimes (x (get-board-width board))
      (if (not (is-field-set board x 0)) (setf move-left t))
      )
  move-left
  ))

(defun get-random-move (moves filter-score-value)
  "Chooses a random move from all moves that have same the score as the given one. moves: list of tupels (x y score). filter-ccore-value: Reference score"
  (setf moves (remove-if-not (lambda (move) (equal filter-score-value (third move))) moves))
  (let ((index (/ (random (* 1000 (length moves))) 1000)))
    (nth (floor index) moves)
    ))

(defun reduce-scores (moves is-opponent &key (skip-randomizer nil))
  "Reduce list of possible moves."
  ;; moves: list of tupels (x y score)
  ;; is-opponent: t -> score will be minimized, nil -> score will be maximized
  ;; Maximize: #'> Minimize: #'<
  ;; skip-randomizer: nil -> If multiple moves are available choose a random one. t -> choose first one
  ;; returns move with minimum or maximum score
  (let ((move nil) (fn (if is-opponent #'< #'>)))
    (setf move (reduce (lambda (best item)
			 (if (funcall fn (third item) (third best)) item best)) 
		       moves))
    (if (and move (not skip-randomizer) (not *engine-configuration-skip-randomizer*))
	(get-random-move moves (third move))
      move)
    ))

;;; returns a tupel (x y score) where x represents the column, y the row and score the score of the column
;;; max-depth: Maximum number of half-moves to execute (1..n)
;;; color: The computers color
;;;
;;; create a clone of the board that for performance reasons will be manipulated during the traversal
(defun minmax (the-board color max-depth &key (print-engine-configuration nil))
  "Minimax implementation. Calculates a counter move. max-depth >= 1"
  (if print-engine-configuration
      (print-engine-configuration)
      )
  (let ((board (clone-board the-board)) (result nil))
    ;; cur-depth >= 1
    (labels ((minmax-inner (board color is-opponent cur-depth)
	       (let ((generated-moves (generate-moves board))  (moves ()) (score nil) (is-four nil) (quit-loop nil)   )
		 (dolist (move generated-moves)
		   (if quit-loop
		       nil ;; (progn (format t "Schleife abgebrochen"))
		       (progn
			 (nset-field board (first move) (second move) color) ; do move
			 (setf score (board-score board (first move) (second move) )) ; calc score
			 (setf is-four (>= score 1.0)) ; 4 pieces in a row?
			 (if (and *engine-configuration-quit-row-evaluation-on-four* is-four)
			     (setf quit-loop t))
			 (if is-opponent (setf score (* -1.0 score))) ; invert score if opponents draw
			 (if *engine-configuration-depth-relative-score*
			     (setf score (/ score (expt 10 (- cur-depth 1)))))
			 ;; final state or no more moves availabe or max depth reached
			 (if (or is-four (not (is-move-available board)) (equal cur-depth max-depth))
			     (progn
			       (push (list (first move) (second move) score) moves)
			       )
			     (progn
			       (setf score (minmax-inner board (invert-color color) (not is-opponent) (+ cur-depth 1)))
			       (push (list (first move) (second move) (third score)) moves)))
			 (nset-field board (first move) (second move) EMPTY) ; undo move
			 )
		       ))
		 (let ((result (reduce-scores moves is-opponent :skip-randomizer (if (equal cur-depth 1) nil t))))
		   (funcall *engine-notification-reduced-scores* board color is-opponent cur-depth result moves)
		   result)
		 )))
      (setf result (minmax-inner board color nil 1))
      (if (not (equalp board the-board))
	  (progn 
	    (format t "~%FATAL ERROR: BOARDS ARE NOT EQUAL~%")
	    (format t "Original board: ~%")
	    (format-board (make-instance 'board-formatter) the-board)
	    (format t "~%Play board: ~%")
	    (format-board (make-instance 'board-formatter) board)
	    (format t "~%")
	    ))
      result
      )))

