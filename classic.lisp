

#|
Implementation of the classic 4-Connect game
|#

#|
Evaluate the score of the board
x y: The latest move
|#
(defun board-score (board x y)
  (let ((length (max-line-length-at board x y)))
    (if (>= length 4) 1.0 0.0)
    ))


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


;; returns x y and score, e.g. (0 3 1.0) or nil
(defun best-move (board color)
  (let ((final-score (get-minmax board color nil 2)))
    ;;    (print "Final Score")
    ;;(print final-score)
    (if final-score 
	(list (first final-score) (+ (line-length-at board (first final-score) 0 0 1) -1) (second final-score))
      nil)
    ))

;; returns  (x min/max-score)
(defun get-minmax (board color is-opponent max-depth)
  (let ((moves (generate-moves board)) (cur-board nil) (scores ()) (score nil) (next-moves nil))
    (dolist (move moves)
	     ;; do move
	     (setf cur-board (set-field board (first move) (second move) color))
	     ;; calc score and next move in order to decide if a final state has been reached
	     (setf score (board-score cur-board (first move) (second move) ))
	     ;; invert if opponent
	     (if is-opponent (setf score (* -1 score)))
	     ;; todo: in die if-abfrage reinziehen
	     (setf next-moves (generate-moves cur-board))
	     ;; final state or no more moves or max depth
	     (if (or (equal score 1.0) (equal score -1.0) (not next-moves) (equal max-depth 0))
		 (progn
		   (push (list (first move) score) scores))
	       (progn
		 (setf score (get-minmax cur-board (invert-color color) (not is-opponent) (+ max-depth -1)))
		 (push (list (first move) (second score)) scores)))
	     )
    ;; now we have a list of (x score) tuples. Lets max/min them
    ;; Todo: progn raus
    (if is-opponent
	    (progn
	      ;; (print "Minimizing")
	      (setf score (reduce (lambda (best item)
				    (if (< (second item) (second best)) item best))
				  scores))
	      score
	      )
	  (progn
	    ;; (print "Maximizing")
	    (setf score (reduce (lambda (best item)
				    (if (> (second item) (second best)) item best))
				  scores))
	    score
	    )
	  )))

