
#|
Accessor functions for the result returned by engine:play
|#


(in-package :playresult)

(defun play-result-column (result)
  (first result))

(defun play-result-row (result)
  (second result))

(defun play-result-score (result)
  (third result))

(defun play-result-no-move-available (result)
  (not (first result)))

(defun play-result-move-sequence (result)
  (fourth result))

(defun play-result-players-color (result)
  (second (first (fourth result))))

(defun play-result-filter-move-sequence-by-token (result token)
  (remove-if-not
   (lambda (item) (eql (second item) token))
   (play-result-move-sequence result)))

(defun play-result-players-move-sequence (result)
  (if (not (play-result-no-move-available result))
      (mapcar
       (lambda (item) (first item))
       (play-result-filter-move-sequence-by-token
	result
	(play-result-players-color result)))
      nil))

(defun play-result-is-four-1 (result)
  (equal (third (first (fourth result))) "MATE"))
  
(defun play-result-is-four-n (result)
  (let ((token (play-result-players-color result)))
    (remove-if-not
     (lambda (item) (equal (third item) "MATE"))
     (play-result-filter-move-sequence-by-token result token))))

