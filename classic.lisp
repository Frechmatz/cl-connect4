

#|
Implementation of the classic 4-Connect game
|#

#|
Evaluate the score of the board
x y: The latest move
|#
(defun board-score (board x y)
  (setf length (max-line-length-at board x y))
  (if (>= length 4) 1.0 0.0)
    )


#|
Returns a list of (x y) coordinates of all possible moves
board: The board
|#
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


(defun is-move-available (board)
  (setf move-left nil)
  (dotimes (x *WIDTH*)
    (if (not (is-field-set board x 0)) (setf move-left t))
    )
  move-left
  )


;; returns x y and score, e.g. (0 3 1.0) or nil
(defun best-move (board color max-depth)
  (let ((final-score (get-minmax board color nil 0 max-depth)))
    ;;(print "Final Score")
    ;;(print final-score)
    (if final-score 
	(list (first final-score) (+ (line-length-at board (first final-score) 0 0 1) -1) (second final-score))
      nil)
    ))

;; reduces and chooses a random score from all scores that have same the score-value as the reduced one 
(defun get-random-move (moves filter-score-value)
  (setf moves (remove-if-not (lambda (move) (equal filter-score-value (second move))) moves))
  ;;(print "Candidates of randmizer:")
  ;;(print moves)
  (let ((index (/ (random (* 1000 (length moves))) 1000)))
    ;;(format t "Index: ~a~%" index)
    (nth (floor index) moves)
    ))


;; moves: list of tupels (x score)
;; is-opponent: t -> minimize, nil -> maximize
;; Maximize: #'> Minimize: #'<
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


;; returns resulting tupel (x score)
(defun get-minmax (the-board color is-opponent cur-depth max-depth)
  ;; create clone of board that for performance reasons will be manipulated during the traversal
  (let ((board (clone-board the-board)))
    (labels ((get-minmax-inner (board color is-opponent cur-depth max-depth)
				(let ((moves (generate-moves board))  (scores ()) (score nil) (next-moves nil))
				  (dolist (move moves)
				    (nset-field board (first move) (second move) color) ;; do move
				    (setf score (board-score board (first move) (second move) )) ;; calc score
				    (setf is-four (equal score 1.0)) ;; 4 pieces in a row? 
				    (setf score (/ score (+ cur-depth 1.0))) ;; adapt score to current search depth
				    (if is-opponent (setf score (* -1.0 score))) ;; invert score if opponents draw
				    ;; final state or no more moves availabe or max depth reached
				    (if (or is-four (not (is-move-available board)) (equal max-depth 0))
					(progn
					  (push (list (first move) score) scores)
					  )
				      (progn
					(setf score (get-minmax-inner board (invert-color color) (not is-opponent) (+ cur-depth 1) (+ max-depth -1)))
					(push (list (first move) (second score)) scores)))
				    (nset-field board (first move) (second move) *EMPTY*) ;; undo move
				    )
				  ;; now we have a list of (x score) tuples. Lets max/min them
				  (reduce-scores scores is-opponent)
				  )))
	    ;; body of labels
	    (get-minmax-inner board color is-opponent cur-depth max-depth)
	    )))


