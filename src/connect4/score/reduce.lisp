

(in-package :score)

(defvar *column-weights* nil
  "This array defines the weight of each column of the current board. 0 > weight <= 1.0")


;;; Initialize 'seed' of random number generator.
(setf *random-state* (make-random-state t))

(defun get-random-entry (moves)
  "Chooses a random entry of the given list."
  (if (equal 1 (length moves))
      (first moves)
      (let ((index (/ (random (* 1000 (length moves))) 1000)))
    (nth (floor index) moves)
    )))

(defun get-reduced-scores (moves is-opponent)
  "Get list of all moves that belong to max/min score of the given moves. 
moves must not be nil
is-opponent: t -> score will be minimized, nil -> score will be maximized
Maximize: #'> Minimize: #'<"
  (if (equal 1 (length moves))
      moves
      (let ((comparison-fn (if is-opponent #'< #'>)))
	(let ((move (reduce (lambda (best item)
			      (if (funcall comparison-fn (third item) (third best)) item best)) 
			    moves)))
	  (remove-if-not (lambda (cur-move) (equal (third move) (third cur-move))) moves)
	  ))))

(defun get-max-column-weighted-moves (moves)
  (if (not *column-weights*)
      (error 'internal-error :text "get-max-column-weighted-moves: column-weights not set"))
  (if (equal 1 (length moves))
      moves
      (let ((comparison-fn #'>))
	(let ((move (reduce (lambda (best item)
			      (if (funcall comparison-fn
					   (aref *column-weights* (first item))
					   (aref *column-weights* (first best)))
				  item
				  best))
			      moves)))
	  (remove-if-not
	   (lambda (cur-move) (equal
			       (aref *column-weights* (first cur-move))
			       (aref *column-weights* (first move))))
	   moves)))))

(defun reduce-scores (moves is-opponent &key (skip-randomizer nil) (skip-prefer-center nil))
  "Reduce list of possible moves.
  moves: list of tupels (x y score)
  skip-randomizer: nil -> If multiple moves are available choose a random one. t -> choose first one
  returns move with minimum or maximum score"
  (if (not moves) ;; reduce doesn't like empty lists
      nil
      (progn 
	(let ((resulting-moves (get-reduced-scores moves is-opponent)))
	  (if (not skip-prefer-center)
	      (setf resulting-moves (get-max-column-weighted-moves resulting-moves)))
	  (if (not skip-randomizer)
	      (get-random-entry resulting-moves)
	      (first resulting-moves))
	  ))))





