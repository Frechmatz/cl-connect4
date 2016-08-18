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
  (slot-value result 'column))

(defun play-result-row (result)
  (slot-value result 'row))

(defun play-result-score (result)
  (slot-value result 'score))

(defun play-result-is-move (result)
  (slot-value result 'row))

(defun play-result-move-sequence (result)
  (slot-value result 'move-sequence))

(defun play-result-players-color (result)
  (slot-value result 'color))

(defun play-result-filter-move-sequence-by-token (result token)
  (remove-if-not
   (lambda (item) (eql (second item) token))
   (play-result-move-sequence result)))

(defun play-result-players-move-sequence (result)
  (if (play-result-is-move result)
      (mapcar
       (lambda (item) (first item))
       (play-result-filter-move-sequence-by-token
	result
	(play-result-players-color result)))
      nil))

(defun play-result-is-four-1 (result)
  (equal (third (first (play-result-move-sequence result))) "MATE"))
  
(defun play-result-is-four-n (result)
  (let ((token (play-result-players-color result)))
    (remove-if-not
     (lambda (item) (equal (third item) "MATE"))
     (play-result-filter-move-sequence-by-token result token))))

