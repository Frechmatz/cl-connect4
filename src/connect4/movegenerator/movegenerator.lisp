
(in-package :movegenerator)

(defun generate-moves (board &key (column-filter nil))
  "Generate moves. Returns a list of (x y) coordinates of all possible moves"
  (remove-if-not
   (lambda (i) (if (not column-filter) t (eql column-filter (first i)))) 
   (let ( (moves ()) (row nil))
     (dotimes (x (get-width board))
       (setf row (drop board x))
       (if row (push (list x row) moves)))
     moves)))

(defun is-move-available (board)
  "Low cost function to check if at least one move is available"
  (let (( move-left nil))
    (dotimes (x (get-width board))
      (if (not (field-set-p board x 0)) (setf move-left t)))
    move-left))

