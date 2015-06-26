

#|
Implementation of the classic 4-Connect game
|#

;;
;; Evaluate the score of the board
;; x y: The latest move
(defun board-score (board x y)
  (setf length (max-line-length-at board x y))
  (if (>= length 4) 1.0 0.0)
    )


;;
;; Returns a list of (x y) coordinates of all possible moves
;; board: The board
(defun generate-moves (board)
  (let ( (moves ()) (depth 0))
    (dotimes (x *WIDTH*)
      (if (not (is-field-set board x 0))
	  (progn
	    ;; todo: zusammenfassen und lokale Variable weg
	    (setf depth (line-length-at board x 0 0 1))
	    (push (list x (- depth 1)) moves)
	    )
	)
      )
    moves
    ))

;;
;; Check if a move is available for the given board
;; Todo: Optimize
(defun is-move-available (board)
  (setf move-left nil)
  (dotimes (x *WIDTH*)
    (if (not (is-field-set board x 0)) (setf move-left t))
    )
  move-left
  )

;;
;; Calculate counter-move. Entry point for the game repl
;; returns x y and score, e.g. (0 3 0.75) or nil
(defun best-move (board color max-depth)
  (let ((final-score (get-minmax board color nil 0 max-depth)))
     (if final-score 
	(list (first final-score) (+ (line-length-at board (first final-score) 0 0 1) -1) (second final-score))
      nil)
    ))

;;
;; Chooses a random move from all moves that have same the score as the given one 
(defun get-random-move (moves filter-score-value)
  (setf moves (remove-if-not (lambda (move) (equal filter-score-value (second move))) moves))
  (let ((index (/ (random (* 1000 (length moves))) 1000)))
    (nth (floor index) moves)
    ))


;;
;; Reduce list of possible moves
;; moves: list of tupels (x score)
;; is-opponent: t -> score will be minimized, nil -> score will be maximized
;; Maximize: #'> Minimize: #'<
;; skip-randomizer: nil -> If there are multiple moves available choose a random one. t -> choose first one
;; returns move with minimum or maximum score
(defun reduce-scores (moves is-opponent &optional skip-randomizer)
  (let ((move nil) (fn (if is-opponent #'< #'>)))
    (setf move (reduce (lambda (best item)
			 (if (funcall fn (second item) (second best)) item best)) 
		       moves))
    (if (and move (not skip-randomizer))
	(get-random-move moves (second move))
      move)
    ))


;;
;; Minimax implementation
;; returns a tupel (x score) where x represents the column and score the score for the column
(defun get-minmax (the-board color is-opponent cur-depth max-depth)
  ;; create a clone of the board that for performance reasons will be manipulated during the traversal
  (let ((board (clone-board the-board)))
    (labels ((get-minmax-inner (board color is-opponent cur-depth max-depth)
				(let ((generated-moves (generate-moves board))  (moves ()) (score nil) (next-moves nil))
				  (dolist (move generated-moves)
				    (nset-field board (first move) (second move) color) ;; do move
				    (setf score (board-score board (first move) (second move) )) ;; calc score
				    (setf is-four (equal score 1.0)) ;; 4 pieces in a row? 
				    (setf score (/ score (+ cur-depth 1.0))) ;; adapt score to current search depth
				    (if is-opponent (setf score (* -1.0 score))) ;; invert score if opponents draw
				    ;; final state or no more moves availabe or max depth reached
				    (if (or is-four (not (is-move-available board)) (equal max-depth 0))
					(progn
					  (push (list (first move) score) moves)
					  )
				      (progn
					(setf score (get-minmax-inner board (invert-color color) (not is-opponent) (+ cur-depth 1) (+ max-depth -1)))
					(push (list (first move) (second score)) moves)))
				    (nset-field board (first move) (second move) *EMPTY*) ;; undo move
				    )
				  ;; now we have a list of (x score) tuples. Reduce them to a final move
				  (reduce-scores moves is-opponent)
				  )))
	    (get-minmax-inner board color is-opponent cur-depth max-depth)
	    )))


