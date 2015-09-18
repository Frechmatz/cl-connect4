;;;;
;;;; Engine of classic Connect4
;;;;

(in-package :connect4)

;;;
;;; Condition that signals an implementation error of the engine
;;;
(define-condition internal-error (error)
  ((text :initarg :text :reader text)))


(defconstant CLASSIC-WIDTH 7 "Board width of the original game") 
(defconstant CLASSIC-HEIGHT 6 "Board height of the original game") 

(defvar *engine-notification-reduced-scores*
  (lambda (board color is-opponent depth reduced-score all-scores)
  (declare (ignore board color is-opponent depth reduced-score all-scores))
  nil)
  "Handler that is called each time after a minimizing/maximizing of a row of scores took place")

(defvar *engine-configuration-prefer-center* nil
  "Experimental. Prefer moves that are near to the horizontal center of the board.")


(defun print-engine-configuration ()
  (format t "Engine configuration:")
  (format t "*engine-configuration-prefer-center*: ~a" *engine-configuration-prefer-center*)
  (format t "~%")
  )

(defvar *column-weights* nil
  "This array defines the weight of each column of the current board. 0 > weight <= 1.0"
  )

(defun calc-column-weights (board-width prefer-center)
  "Calculate a weight for each column. The nearer to the center the higher the weight"
  (let ((weights (make-array board-width)))
    (dotimes (x board-width)
      (if (not prefer-center)
	  (setf (aref weights x) 1.0)
	  ;; 1 / (1 + Distance from center)
	  (setf (aref weights x) (/ 1.0 (+ 1 (abs (- (/ board-width 2) x)))))
	  ))
    weights))

(defun board-score (board x y)
  "Evaluate the score of the board. x y: The current move. Returns a value 0 >= value <= 1, where 1 signals a winning position"
  (let ((l (max-line-length-at board x y (get-field board x y))))
    (if (>= l 4)
	1.0
	(progn
	  (if (>= l 3) 0.5 0.0))
	)
    ))

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
  (if (not moves) ; reduce doesn't like empty lists
      nil
      (progn 
	(let ((move nil) (fn (if is-opponent #'< #'>)))
	  (setf move (reduce (lambda (best item)
			       (if (funcall fn (third item) (third best)) item best)) 
			     moves))
	  (if (not skip-randomizer)
	      (get-random-move moves (third move))
	      move)
	  ))))

(defun peek-is-four (moves board color)
  "Check all moves if an immediate four is present. If yes returns a list consisting of such move otherwise return the moves given into function"
  (let ((four-move nil) (score nil))
    (dolist (move moves)
      (if (not four-move)
	  (progn 
	    (nset-field board (first move) (second move) color) ; do move
	    (setf score (board-score board (first move) (second move))) ; calc score
	    (nset-field board (first move) (second move) EMPTY) ; undo move
	    (if (>= score 1.0) ;; 4 pieces in a row?
		(progn
		  (setf four-move move)
		  )
		))))
    (if four-move
	(list four-move)
	moves)))
  
;;;
;;; returns a tupel (x y score) where x represents the column, y the row and score the score of the column
;;; max-depth: Maximum number of half-moves to execute (1..n)
;;; color: The computers color
;;;
;;; create a clone of the board that for performance reasons will be manipulated during the traversal
(defun minmax (the-board color max-depth &key (print-engine-configuration nil))
  "Minimax implementation. Calculates a counter move. max-depth >= 1"
  (if print-engine-configuration
      (print-engine-configuration))
  (let ((board (clone-board the-board)) (result nil)
	(*column-weights* (calc-column-weights (connect4::get-board-width the-board)
					       *engine-configuration-prefer-center*))
	)
    ;; cur-depth >= 1
    (labels ((minmax-inner (board color is-opponent cur-depth)
	       (let ((generated-moves (generate-moves board))  (moves ()) (score nil) (is-four nil))
		 (setf generated-moves (peek-is-four generated-moves board color))
		 (dolist (move generated-moves)
		   (progn
		     (nset-field board (first move) (second move) color) ; do move
		     (setf score (board-score board (first move) (second move) )) ; calc score
		     (setf is-four (>= score 1.0)) ; 4 pieces in a row?
		     (if is-opponent (setf score (* -1.0 score))) ; invert score if opponents draw
		     (setf score (/ score (expt 10 (- cur-depth 1))))
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
		   )
		 (let ((result (reduce-scores moves is-opponent :skip-randomizer (if (equal cur-depth 1) nil t))))
		   (funcall *engine-notification-reduced-scores* board color is-opponent cur-depth result moves)
		   result)
		 )))
      (setf result (minmax-inner board color nil 1))
      (if (not (equalp board the-board))
	  (progn
	    (format t "~%Fatal error: Temporary board is not equal to incoming one~%")
	    (format t "Original board: ~%")
	    (format-board (make-instance 'board-formatter) the-board)
	    (format t "~%Temporary board: ~%")
	    (format-board (make-instance 'board-formatter) board)
	    (format t "~%")
	    ;; Game Over
	    (error 'internal-error :text "Temporary board is not equal to the incoming one")
	    ))
      result
      )))

