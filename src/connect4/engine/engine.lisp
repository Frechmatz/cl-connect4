;;;;
;;;; Engine of classic Connect4
;;;;

(in-package :engine)

;; (defconstant MATE "MATE" "Mate condition determined") 

;;;
;;; Condition that signals an implementation error of the engine
;;;
(define-condition internal-error (error)
  ((text :initarg :text :reader text)))

(defvar *engine-notification-reduced-scores*
  (lambda (board color is-opponent depth reduced-score all-scores)
  (declare (ignore board color is-opponent depth reduced-score all-scores))
  nil)
  "Handler that is called each time after a minimizing/maximizing of a row of scores took place")

(defvar *engine-configuration-prefer-center* t
  "Prefer moves that are near to the horizontal center of the board.")

(defun toggle-color (color)
  (if (eq color WHITE) BLACK WHITE))

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
  "Evaluate the score of the board. x y: The current move. 
Returns a value 0 >= value <= 1, where 1 signals a winning position"
  (if (not *column-weights*)
      (error 'internal-error :text "board-score: column-weights not set"))
  (if (>= x (get-width board))
      (error 'internal-error :text "board-score: x out of range"))
  (let ((l (length (get-connected-pieces board x y))))
    (if (>= l 4)
	1.0
	(progn
	  (if (>= l 3)
	      (+ 0.5 (* 0.49 (aref *column-weights* x))) ; 0.50 .. 0.99
	      (+ 0.25 (* 0.24 (aref *column-weights* x))) ; 0.25 .. 0.49
	)))))

(defun generate-moves (board &key (column-filter nil))
  "Generate moves. Returns a list of (x y) coordinates of all possible moves"
  (remove-if-not
   (lambda (i) (if (not column-filter) t (eql column-filter (first i)))) 
   (let ( (moves ()) (row nil))
     (dotimes (x (get-width board))
       (setf row (drop board x))
       (if row (push (list x row) moves))
       )
     moves)))

;;; Internal method for fast check if a move is available for given board
(defun is-move-available (board)
  (let (( move-left nil))
    (dotimes (x (get-width board))
      (if (not (field-set-p board x 0)) (setf move-left t))
      )
  move-left))


(defun peek-is-four (moves board color)
  "Check all moves if an immediate four is present. 
If t returns a list consisting of such move otherwise return the moves given into function"
  (let ((four-move nil) (score nil))
    (dolist (move moves)
      (if (not four-move)
	  (progn 
	    (nset-field board (first move) (second move) color) ; do move
	    (setf score (board-score board (first move) (second move))) ; calc score
	    (nclear-field board (first move) (second move)) ; undo move
	    (if (>= score 1.0) ;; 4 pieces in a row?
		(progn
		  (setf four-move move)
		  )
		))))
    (if four-move
	(list four-move)
	moves)))
  
;;;
;;; Returns a tupel (x y score line) where
;;; x: represents the column,
;;; y: the row,
;;; score: the score of the column
;;; line: a list of moves. Each move consists of a list
;;;     (x color status) The status value "MATE" indicates a mate situation. 
;;; max-depth: Maximum number of half-moves to execute (1..n)
;;; color: The computers color
;;;
;;; create a clone of the board that for performance reasons will be manipulated during the traversal
(defun play (the-board color max-depth &key (start-column nil))
  "Minimax implementation. Calculates a counter move. max-depth >= 1"
  (let (
	(board (clone-board the-board))
	(result nil)
	(cur-line '())
	(column-filter start-column)
	(*column-weights*
	 (calc-column-weights
	  (get-width the-board)
	  *engine-configuration-prefer-center*)))
    ;; cur-depth >= 1
    (labels ((minmax-inner (board color is-opponent cur-depth)
	       (let (
		     (generated-moves (generate-moves board :column-filter column-filter))
		     (moves ())
		     (score nil)
		     (is-four nil))
		 (setf column-filter nil)
		 (setf generated-moves (peek-is-four generated-moves board color))
		 (dolist (move generated-moves)
		   (progn
		     (nset-field board (first move) (second move) color) ; do move
		     (setf score (board-score board (first move) (second move) )) ; calc score
		     (setf is-four (>= score 1.0)) ; 4 pieces in a row?
		     (if is-opponent (setf score (* -1.0 score))) ; invert score if opponents draw
		     (setf score (/ score (expt 10 (- cur-depth 1))))
		     (push (list (first move) color (if is-four "MATE" nil)) cur-line)
		     ;; final state or no more moves availabe or max depth reached
		     (if (or is-four (not (is-move-available board)) (equal cur-depth max-depth))
			 (progn
			   (push (list (first move) (second move) score (copy-list cur-line)) moves)
			   )
			 (progn
			   (setf score (minmax-inner board (toggle-color color) (not is-opponent) (+ cur-depth 1)))
			   (push (list (first move) (second move) (third score) (fourth score)) moves)))
		     (nclear-field board (first move) (second move)) ; undo move
		     (setf cur-line (cdr cur-line))
		     )
		   )
		 (let ((result (score:reduce-scores moves is-opponent :skip-randomizer (if (equal cur-depth 1) nil t))))
		   (funcall *engine-notification-reduced-scores* board color is-opponent cur-depth result moves)
		   result)
		 )))
      (setf result (minmax-inner board color nil 1))
      ;; revert best line
      (setf result (list (first result) (second result) (third result) (reverse (fourth result))))
      (if (not (equalp board the-board))
	  (progn
	    (format t "~%Fatal error: Temporary board is not equal to incoming one~%")
	    (format t "Original board: ~%")
	    (format t the-board)
	    (format t "~%Temporary board: ~%")
	    (format t board)
	    (format t "~%")
	    ;; Game Over
	    (error 'internal-error :text "Temporary board is not equal to the incoming one")
	    ))
      result
      )))

