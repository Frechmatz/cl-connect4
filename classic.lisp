

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
(defun best-move (board color)
  (let ((final-score (get-minmax board color nil 0 3)))
    ;;(print "Final Score")
    ;;(print final-score)
    (if final-score 
	(list (first final-score) (+ (line-length-at board (first final-score) 0 0 1) -1) (second final-score))
      nil)
    ))

;; reduces and chooses a random score from all scores that have same the score-value as the reduced one 
(defun reduce-scores-random (testFn scores &optional skip-randomizer)
  (setf score (reduce testFn scores))
  ;; Todo: Randomize
  score)



;; returns  (x min/max-score)
(defun get-minmax (board color is-opponent cur-depth max-depth)
  #|
  (print "get-minmax called")
  (print  color)
  (print is-opponent)
  (print cur-depth)
  (print max-depth)
  |#
  (let ((moves (generate-moves board)) (cur-board nil) (scores ()) (score nil) (next-moves nil))
    (dolist (move moves)
	     ;; do move
	     (setf cur-board (set-field board (first move) (second move) color))
	     ;; calc score
	     (setf score (board-score cur-board (first move) (second move) ))
	     ;; 4 pieces in a row?
	     (setf is-four (equal score 1.0)) 
	     ;; adapt score to current search depth
	     (setf score (/ score (+ cur-depth 1.0)))
	     ;; invert if opponent
	     (if is-opponent (setf score (* -1.0 score)))
	     ;; final state or no more moves availabe or max depth reached
	     (if (or is-four (not (is-move-available board)) (equal max-depth 0))
		 (progn
		   (push (list (first move) score) scores))
	       (progn
		 (setf score (get-minmax cur-board (invert-color color) (not is-opponent) (+ cur-depth 1) (+ max-depth -1)))
		 (push (list (first move) (second score)) scores)))
	     )
    ;; now we have a list of (x score) tuples. Lets max/min them
    ;; Todo: progn raus
    (if is-opponent
	(progn
	  ;;(print "Minimizing")
	  ;;(format-board board (make-instance 'colorful-cell-formats))
	  ;;(print scores)
	  (setf score (reduce-scores-random (lambda (best item)
					      (if (< (second item) (second best)) item best))
					    scores))
	  ;;(print "Minimized Score:")
	  ;;(print score)
	  score
	  )
      (progn
	;;(print "Maximizing")
	;;(format-board board (make-instance 'colorful-cell-formats))
	;;(print scores)
	(setf score (reduce-scores-random (lambda (best item)
					    (if (> (second item) (second best)) item best))
					  scores))
	;;(print "Maximized Score:")
	;;(print score)
	score
	)
      )))

