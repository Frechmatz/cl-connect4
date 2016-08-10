

(in-package :connect4-test)

;;;;
;;;; Next move by: WHITE
;;;;
;;;; This board presents a situation where WHITE will in two moves
;;;; by throwing into column 4
;;;; Requires a traversal depth >= 3 half-moves
;;;;

(defun create-board-02 ()
  (create-test-board (list
		      "wb...."
		      "bbww.w"
		      "bbww.w"
		      "bwbb.."
		      )))

;;; test with traversal depth 2 (win situation won't be recognized)
(define-test test-board-02-a ()
	     (let ((result
		    (engine:play
		     (create-board-02) board:WHITE 2)))
	       (assert-played-column result '(2 3 4 5))
	       (assert-is-not-mate result)))

;;; test with traversal depth 3 (win situation will be recognized)
(define-test test-board-02-b ()
	     (let ((result
		    (engine:play
		     (create-board-02) board:WHITE 3)))
	       (assert-played-column result '(4))
	       (assert-is-mate result)))
