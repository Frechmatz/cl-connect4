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

(defvar *column-weights* nil
  "This array defines the weight of each column of the current board. 0 > weight <= 1.0")

(defun toggle-color (color)
  (if (eq color WHITE) BLACK WHITE))

(defun calc-column-weights (board-width)
  "Calculate a weight for each column. The nearer to the center the higher the weight"
  (let ((weights (make-array board-width)))
    (dotimes (x board-width)
      ;; 1 / (1 + Distance from center)
      (setf (aref weights x) (/ 1.0 (+ 1 (abs (- (/ board-width 2) x)))))
      )
    ;;(format t "Column weights: ~a" weights)
    weights))

(defun board-score (board x y)
  "Evaluate the score of the board. x y: The current move. 
Returns a value 0 >= value <= 1, where 1 signals a winning position"
  (if (>= x (get-width board))
      (error 'internal-error :text "board-score: x out of range"))
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
	  (/ 1 (+ 1 distance)))
	)))

  
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
(defun play (the-board color max-depth &key (start-column nil) (is-quit-fn (lambda() nil)) (info-fn (lambda() nil)))
  "Minmax implementation. Calculates a counter move. max-depth >= 1"
  (let (
	(board (clone-board the-board))
	(move-count 0)
	(cur-path '())
	(cur-result nil)
	(column-filter start-column)
	(*column-weights* (calc-column-weights (get-width the-board))))
    ;; cur-depth >= 1
    (labels ((minmax-inner (board color is-opponent cur-depth)
	       (let ((fn (funcall info-fn)))
		 (if fn
		     (funcall fn (list (list :plies move-count)))))
	       (if (and cur-result (funcall is-quit-fn))
		   cur-result
		   (let ((row-scores ()))
		     (let ((next-moves (movegenerator:generate-moves board :column-filter column-filter)))
		       (setf column-filter nil)
		       (if (not next-moves)
			   nil ;;; no moves to play, giving up...
			   (dolist (move next-moves)
			     (let ((x (first move)) (y (second move)))
			       (setf move-count (+ move-count 1))
			       (nset-field board x y color) ; do move
			       (let* ((score (board-score board x y)) (is-four (>= score 1.0)))
				 (if is-opponent (setf score (* -1.0 score))) ; invert score if opponents draw
				 (setf score (/ score (expt 10 (- cur-depth 1))))
				 (push (list x color (if is-four "MATE" nil)) cur-path)
				 ;; final state or max depth reached
				 (if (or is-four (equal cur-depth max-depth))
				     (push (list x y score (copy-list cur-path)) row-scores)
				     (let ((minmax-result (minmax-inner board (toggle-color color) (not is-opponent) (+ cur-depth 1))))
				       (if minmax-result
					   (push (list x y (third minmax-result) (fourth minmax-result)) row-scores)
					   ;; inner call has given up. use previous result
					   (push (list x y score (copy-list cur-path)) row-scores)
					   ))))
			       (nclear-field board x y) ; undo move
			       (setf cur-path (cdr cur-path))))))
		     (let ((result (reduce:reduce-scores
				    row-scores
				    is-opponent
				    (lambda (m) (third m)) ;; Score getter
				    (lambda (m) (aref *column-weights* (first m))) ;; Weight getter
				    :skip-randomizer (if (equal cur-depth 1) nil t))))
		       (setf cur-result result)
		       result)
		     ))))
      (let ((result (minmax-inner board color nil 1)))
	;; revert best line
	;;(list (first result) (second result) (third result) (reverse (fourth result)))
	(make-instance 'playresult
		       :color color
		       :column (first result)
		       :row (second result)
		       :score (third result)
		       :move-sequence (reverse (fourth result)))
	))))

