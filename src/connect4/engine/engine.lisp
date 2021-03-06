;;;;
;;;; Engine of classic Connect4
;;;;

(in-package :engine)

(defun toggle-color (color)
  (if (eq color WHITE) BLACK WHITE))

(defun board-score (board x y)
  "Evaluate the score of the board. 
   x y: The current move. 
   Returns a value 0 >= value <= 1, where 1 signals a winning position"
  (if (>= (length (get-connected-pieces board x y)) 4)
      1.0
      (get-board-variance board (board:get-field board x y))))

(defun play (board color max-depth
	     &key
	       (start-column nil)
	       (is-quit-fn (lambda() nil))
	       (info-fn (lambda() nil)))
  ;; (declare (optimize (debug 3) (speed 0) (space 0)))
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
    game play status is to be provided. Returns a function or nil. If a
    function is returned, it will be called with an 
    assoc list representing some statuses/statistics of the current game play.
    This function is called with a very high frequency. It's up
    to the calling instance to take measures in order to not slow down the 
    calculation.
  Returns an instance of engine:playresult"
  (let ((board-ctrl (make-instance 'board-controller :board board))
	(column-filter start-column))
    (labels ((minmax-inner (color is-opponent cur-depth)
	       (let ((fn (funcall info-fn)))
		 (if fn (funcall fn (list (list :plies (get-count board-ctrl))))))
	       (let ((next-moves (movegenerator:generate-moves
				  (get-board board-ctrl)
				  :column-filter column-filter)))
		 (setf column-filter nil)
		 (if (not next-moves)
		     nil
		     ;; row-scores: List of (x y final-score <path>)
		     ;; where <path> is a List of (x y color static-score)
		     ;; 0 >= static-score <= 1.0 (not depending on current depth)
		     ;; -1.0 >= final-score <= 1.0
		     (let ((row-scores ()))
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
			   (undo-set-boardfield board-ctrl)))
		       (concatenate 'list (reduce:reduce-scores
					   row-scores
					   is-opponent
					   :get-score-fn (lambda (m) (third m))
					   :skip-randomizer (not (equal cur-depth 1)))
				    (list row-scores)))))))
      (let ((result (minmax-inner color nil 1)))
	(make-instance 'playresult
		       :color color
		       :column (first result)
		       :row (second result)
		       :score (third result)
		       :move-sequence (reverse (fourth result))
		       :final-scores (mapcar (lambda (i) (list (first i) (third i))) (fifth result))
		       )))))

