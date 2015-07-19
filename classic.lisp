

#|
Classic-Connect4 specific implementations
|#

;;
;; Board dimensions of the original game
;;
(defparameter *CLASSIC-WIDTH* 7) 
(defparameter *CLASSIC-HEIGHT* 6) 

;;
;; Skip randomizer by setting to not nil.
;; Used for unit tests, to get reproducable results
;;
(defvar *classic-skip-randomizer* nil)

;;
;; Evaluate the score of the board
;; x y: The latest move
;; returns 1.0 or 0.0
;;
(defun board-score (board x y)
  (if (is-four board x y) 1.0 0.0))

;;
;; Move Generator
;; Returns a list of (x y) coordinates of all possible moves
;; board: The board
;;
(defun generate-moves (board)
  (let ( (moves ()) (row nil))
    (dotimes (x (get-board-width board))
      (setf row (find-row board x))
      (if row (push (list x row) moves))
      )
    moves
    ))

;;
;; Check if a move is available for the given board
;; Todo: Optimize
;;
(defun is-move-available (board)
  (let (( move-left nil))
    (dotimes (x (get-board-width board))
      (if (not (is-field-set board x 0)) (setf move-left t))
      )
  move-left
  ))

;;
;; Chooses a random move from all moves that have same the score as the given one
;; Moves list of tupels (x y score)
;;
(defun get-random-move (moves filter-score-value)
  (setf moves (remove-if-not (lambda (move) (equal filter-score-value (third move))) moves))
  (let ((index (/ (random (* 1000 (length moves))) 1000)))
    (nth (floor index) moves)
    ))


;;
;; Reduce list of possible moves
;; moves: list of tupels (x y score)
;; is-opponent: t -> score will be minimized, nil -> score will be maximized
;; Maximize: #'> Minimize: #'<
;; skip-randomizer: nil -> If multiple moves are available choose a random one. t -> choose first one
;; returns move with minimum or maximum score
;;
(defun reduce-scores (moves is-opponent &optional skip-randomizer)
  (let ((move nil) (fn (if is-opponent #'< #'>)))
    (setf move (reduce (lambda (best item)
			 (if (funcall fn (third item) (third best)) item best)) 
		       moves))
    (if (and move (not skip-randomizer) (not *classic-skip-randomizer*))
	(get-random-move moves (third move))
      move)
    ))

;;
;; Minimax implementation
;; returns a tupel (x y score) where x represents the column, y the row and score the score of the column
;;
(defun get-minmax (the-board color is-opponent cur-depth max-depth)
  ;; create a clone of the board that for performance reasons will be manipulated during the traversal
  (let ((board (clone-board the-board)))
    (labels ((get-minmax-inner (board color is-opponent cur-depth max-depth)
	       (let ((generated-moves (generate-moves board))  (moves ()) (score nil) (is-four nil))
		 (dolist (move generated-moves)
		   (nset-field board (first move) (second move) color) ;; do move
		   (setf score (board-score board (first move) (second move) )) ;; calc score
		   (setf is-four (equal score 1.0)) ;; 4 pieces in a row? 
		   (setf score (/ score (+ cur-depth 1.0))) ;; adapt score to current search depth
		   (if is-opponent (setf score (* -1.0 score))) ;; invert score if opponents draw
		   ;; final state or no more moves availabe or max depth reached
		   (if (or is-four (not (is-move-available board)) (equal max-depth 0))
		       (progn
			 (push (list (first move) (second move) score) moves)
			 )
		       (progn
			 (setf score (get-minmax-inner board (invert-color color) (not is-opponent) (+ cur-depth 1) (+ max-depth -1)))
			 (push (list (first move) (second move) (third score)) moves)))
		   (nset-field board (first move) (second move) *EMPTY*) ;; undo move
		   )
		 ;; we now have a list of (x y score) tuples. Reduce them to a final move
		 (reduce-scores moves is-opponent (if (equal cur-depth 0) nil t))
		 )))
      (get-minmax-inner board color is-opponent cur-depth max-depth)
      )))


;;
;; Calculate counter-move. Entry point for the game repl
;; returns tupel (x y score), e.g. (0 3 0.75) or nil
;;
(defun best-move (board color max-depth)
  (get-minmax board color nil 0 max-depth)
  )

