

(in-package :connect4-test)

;;;
;;;
;;; Status: Ok
;;;
;;; Next move by: WHITE
;;;
;;; This board presents a situation where WHITE will in two moves
;;; by throwing into column 4
;;; Requires a traversal depth >= 3 half-moves
;;;
(defun create-board-02 ()
  (create-test-board (list
		      "wb...."
		      "bbww.w"
		      "bbww.w"
		      "bwbb.."
		      )))

;;; test with traversal depth 2 (win situation won't be recognized)
(define-test test-board-02-a ()
  (let ( (board nil) (best-move nil) (connect4::*engine-configuration-skip-randomizer* t))
    (setf board (create-board-02))
    (setf best-move (connect4::minmax board connect4::WHITE 2))
    (assert-equal 2 (first best-move) (format t "test-board02-a: Wrong move chosen: ~a" (first best-move)))
    ))

;;; test with traversal depth 3 (win situation will be recognized)
(define-test test-board-02-b ()
  (let ( (board nil) (best-move nil) (connect4::*engine-configuration-skip-randomizer* t))
    (setf board (create-board-02))
    (setf best-move (connect4::minmax board connect4::WHITE 3))
    (assert-equal 4 (first best-move) (format t "test-board02-b: Wrong move chosen: ~a" (first best-move)))
    ))


