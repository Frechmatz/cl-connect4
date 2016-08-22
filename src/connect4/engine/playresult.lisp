#|
Accessor functions for the result returned by engine:play
|#

(in-package :engine)

(defclass playresult ()
  ((color :initarg :color)
   (column :initarg :column)
   (row :initarg :row)
   (score :initarg :score)
   (move-sequence :initarg :move-sequence)
   ))

(defun play-result-column (result)
  "Get column of the played move"
  (slot-value result 'column))

(defun play-result-row (result)
  "Get row of the played move"
  (slot-value result 'row))

(defun play-result-score (result)
  "Get score of move"
  (slot-value result 'score))

(defun play-result-is-move (result)
  (slot-value result 'row))

(defun play-result-move-sequence (result)
  "Get played move sequence. (first move-sequence) represents the start move"
  (slot-value result 'move-sequence))

(defun play-result-players-color (result)
  (slot-value result 'color))

(defun play-result-filter-move-sequence-by-token (result token)
  (remove-if-not
   (lambda (item) (eql (third item) token))
   (play-result-move-sequence result)))

(defun play-result-players-move-sequence (result)
  "Get the sequence of columns the player has played.
  The first element of the returned sequence represents the start move"
  (if (play-result-is-move result)
      (mapcar
       (lambda (item) (first item))
       (play-result-filter-move-sequence-by-token
	result
	(play-result-players-color result)))
      nil))

(defun play-result-is-four-1 (result)
  "Check if player has a direct Four"
  (equal (fourth (first (play-result-move-sequence result))) 1.0))
  
(defun play-result-is-four-n (result)
  "Check if player will have a Four after some moves"
  (let ((token (play-result-players-color result)))
    (remove-if-not
     (lambda (item) (equal (fourth item) 1.0))
     (play-result-filter-move-sequence-by-token result token))))

