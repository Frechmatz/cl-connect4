
(in-package :connect4-test)


;;;
;;; Next move by: WHITE
;;;
;;; This board presents a situation where BLACK will win if it plays into column 3.
;;; (WHITE is forced to answer with 3 then BLACK will play 4 and the game is over)
;;; To detect and prevent this, a traversal depth > 4 half-moves is necessary.
;;; Expected moves are 3 or 4 or 5 (todo: analyze which counter-move is really the best one)
;;;
(defun create-board-01 ()
  (create-test-board (list
		      "......."
		      "......."
		      "..w...."
		      "..b...."
		      ".wbbbw."
		      "wbbbwww"
		      )))

;;; Let computer calculate a counter move with a traversal depth of 4 half-moves
(define-test test-board-01-computer-fail ()
  (let ( (board nil) (best-move nil) (connect4::*classic-skip-randomizer* t))
    (setf board (create-board-01))
    (setf best-move (connect4::minmax board connect4::WHITE 4))
    (assert-equal 0 (first best-move) (format t "test-board-01-computer-fail: Unexpected move chosen: ~a~%" (first best-move)))
    ))

;;; Let computer calculate a counter move with a traversal depth of 6 half-moves
(define-test test-board-01-computer-pass ()
  (let ( (board nil) (best-move nil) (connect4::*classic-skip-randomizer* t))
    (setf board (create-board-01))
    (setf best-move (connect4::minmax board connect4::WHITE 6))
    (assert-equal 3 (first best-move) (format t "test-board-01-computer-pass: Unexpected move chosen: ~a~%" (first best-move)))
    ))

