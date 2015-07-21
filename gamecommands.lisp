
;;
;; Implementations of the game commands that are executed via the game repl
;;
;;


;; returns new board
(defun game-command-put (board color x y)
  (set-field board x y color)
  )

;;
;; throw a piece into the board
;; returns new board or nil if illegal move
;; cb -> f( x y board)
(defun game-command-throw-piece (board color x cb)
  (let ((y (find-row board x)))
    (if (not y)
	(funcall cb x nil nil)
	(funcall cb x y (set-field board x y color)))))

;;
;; cb -> f( x y score board )
;;
;;
(defun game-command-play (board color difficulty-level cb)
  (let ((result (best-move board color difficulty-level)))
    (if result
	(funcall cb (first result) (second result) (third result) (set-field board (first result) (second result) color))
	(funcall cb nil nil nil nil)
	)))
