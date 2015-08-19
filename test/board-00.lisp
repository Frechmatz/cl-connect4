

(in-package :connect4-test)


;;;
;;; Next move by: WHITE
;;;
;;; This board presents a situation where WHITE will immediately win
;;;
(defun create-board-00 ()
  (create-test-board (list
		      "......."
		      "......."
		      "......."
		      "..w...."
		      "..wbbw."
		      ".bwbwww"
		      )))

;;; Test with traversal depth 1
(define-test test-board-00-a ()
  (let ( (board nil) (best-move nil) (connect4::*classic-skip-randomizer* t))
    (setf board (create-board-00))
    (setf best-move (connect4::minmax board connect4::*WHITE* 1))
    (assert-equal 2 (first best-move) (format t "test-board-00-a: Wrong move chosen: ~a" (first best-move)))
    ))

;;; Test with traversal depth 6
(define-test test-board-00-b ()
  (let ( (board nil) (best-move nil) (connect4::*classic-skip-randomizer* t))
    (setf board (create-board-00))
    (setf best-move (connect4::minmax board connect4::*WHITE* 6))
    (assert-equal 2 (first best-move) (format t "test-board-00-b: Wrong move chosen: ~a" (first best-move)))
    ))




