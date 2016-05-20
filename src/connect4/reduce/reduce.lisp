;;
;; Reduce scores calculated by play() 
;; 

(in-package :reduce)

;;; Initialize 'seed' of random number generator.
(setf *random-state* (make-random-state t))

(defun get-random-entry (moves)
  "Chooses a random entry of the given list."
  (if (equal 1 (length moves))
      (first moves)
      (let ((index (/ (random (* 1000 (length moves))) 1000)))
    (nth (floor index) moves)
    )))

(defun get-reduced-scores (moves is-opponent get-score-fn)
  "Get list of all moves that belong to max/min score of the given moves. 
moves must not be nil
is-opponent: t -> score will be minimized, nil -> score will be maximized
Maximize: #'> Minimize: #'<"
  (if (equal 1 (length moves))
      moves
      (let ((comparison-fn (if is-opponent #'< #'>)))
	;; determine best score
	(let ((move (reduce (lambda (best item)
			      (if (funcall comparison-fn (funcall get-score-fn item) (funcall get-score-fn best)) item best)) 
			    moves)))
	  ;; filter away items not having best-score
	  (remove-if-not (lambda (cur-move) (equal (funcall get-score-fn move) (funcall get-score-fn cur-move))) moves)
	  ))))

(defun get-max-weighted-moves (moves get-weight-fn)
  (if (equal 1 (length moves))
      moves
      (let ((comparison-fn #'>))
	(let ((move (reduce (lambda (best item)
			      (if (funcall comparison-fn
					   (funcall get-weight-fn item)
					   (funcall get-weight-fn best))
				  item
				  best))
			      moves)))
	  (remove-if-not
	   (lambda (cur-move) (equal
			       (funcall get-weight-fn cur-move)
			       (funcall get-weight-fn move)))
	   moves)))))


(defun reduce-scores (moves is-opponent get-score-fn get-weight-fn &key (skip-randomizer nil))
  "Reduce list of possible moves.
  moves: list of moves
  get-score-fn: function that returns the score of a move
  get-weight-fn: function that returns the weight of a move. The weight is used to
  distinguish between same scored moves.
  skip-randomizer: nil -> If multiple moves are available choose a random one. t -> choose first one
  returns move with minimum or maximum score"
  (if (not moves)
      nil
      (progn 
	(let ((resulting-moves (get-reduced-scores moves is-opponent get-score-fn)))
	  (setf resulting-moves (get-max-weighted-moves resulting-moves get-weight-fn))
	  (if (not skip-randomizer)
	      (get-random-entry resulting-moves)
	      (first resulting-moves))
	  ))))

