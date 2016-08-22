;;;;
;;;; Engine of classic Connect4
;;;;

(in-package :engine)

(defun toggle-color (color)
  (if (eq color WHITE) BLACK WHITE))

(defun calc-column-weights (board-width)
  "Calculate a weight for each column. The nearer to the center the higher the weight"
  (let ((weights (make-array board-width)))
    (dotimes (x board-width)
      ;; 1 / (1 + Distance from center)
      (setf (aref weights x) (/ 1.0 (+ 1 (abs (- (/ board-width 2) x))))))
    weights))

(defun board-score (board x y)
  "Evaluate the score of the board. x y: The current move. 
Returns a value 0 >= value <= 1, where 1 signals a winning position"
  (let ((l (length (get-connected-pieces board x y))))
    (if (>= l 4)
	1.0
	;; Plain:
	;; 0.0
	;; For more aggressive play take into account the length of the row.
	;; Note: Preference of moves near the center of the board
	;; is handled by the reduce algorithm. It doesn't make
	;; sense here due to the score propagation of the minmax algorithm
	(let ((distance (- 4 l)))
	  (/ 1 (+ 1 distance))))))

(defun play (board color max-depth
	     &key
	       (start-column nil)
	       (is-quit-fn (lambda() nil))
	       (info-fn (lambda() nil)))
  "Calculate a move.
  board: The board
  color: Computers color
  max-depth: Maximum depth. Value must be >= 1
  start-column: Move generator override for depth = 1. This parameter is kind 
    of a hack and should be replaced by a more general solution.
  is-quit-fn: A function to be called to determine if the game play is to be aborted.
    This function is called with a very high frequency. It's up
    to the calling instance to take measures in order to not slow down the 
    calculation.
  info-fn: A function to be called to determine if information about the current 
    game play status is to be provided. Returns a function or nil. If info-fn
    returns a function, the returned function will be called back with an 
    assoc list representing some statuses/statistics of the current game play.
    This function is called with a very high frequency. It's up
    to the calling instance to take measures in order to not slow down the 
    calculation.
  Returns an instance of engine:playresult"
  (let ((board-ctrl (make-instance 'board-controller :board board))
	(column-filter start-column)
	(column-weights (calc-column-weights (get-width board))))
    (labels ((minmax-inner (color is-opponent cur-depth)
	       (let ((fn (funcall info-fn)))
		 (if fn (funcall fn (list (list :plies (get-count board-ctrl))))))
	       ;; row-scores: List of (x y final-score <path>)
	       ;; where <path> is a List of (x y color static-score)
	       ;; 0 >= static-score <= 1.0 (not depending on current depth)
	       ;; -1.0 >= final-score <= 1.0
	       (let ((row-scores ()))
		 (let ((next-moves (movegenerator:generate-moves
				    (get-board board-ctrl)
				    :column-filter column-filter)))
		   (setf column-filter nil)
		   (if (not next-moves)
		       nil
		       (dolist (move next-moves)
			 (let ((x (first move)) (y (second move)))
			   (set-boardfield board-ctrl x y color)
			   (let* ((score (board-score (get-board board-ctrl) x y)) (is-four (>= score 1.0)))
			     (decorate-path board-ctrl score) 
			     ;; invert score if opponents draw
			     (if is-opponent (setf score (* -1.0 score)))
			     ;; adapt score to current depth
			     (setf score (/ score (expt 10 (- cur-depth 1))))
			     ;; final state or max depth reached or quit
			     (if (or is-four (equal cur-depth max-depth) (funcall is-quit-fn))
				 (push (list x y score (get-path board-ctrl)) row-scores)
				 (let ((minmax-result (minmax-inner
						       (toggle-color color)
						       (not is-opponent)
						       (+ cur-depth 1))))
				   (if minmax-result
				       (push (list x y (third minmax-result) (fourth minmax-result)) row-scores)
				       ;; inner call has given up. use previous result
				       (push (list x y score (get-path board-ctrl)) row-scores)))))
			   (undo-set-boardfield board-ctrl)))))
		 (reduce:reduce-scores
				row-scores
				is-opponent
				:get-score-fn (lambda (m) (third m))
				:get-weight-fn (lambda (m) (aref column-weights (first m)))
				:skip-randomizer (not (equal cur-depth 1))))))
      (let ((result (minmax-inner color nil 1)))
	(make-instance 'playresult
		       :color color
		       :column (first result)
		       :row (second result)
		       :score (third result)
		       :move-sequence (reverse (fourth result)))))))

