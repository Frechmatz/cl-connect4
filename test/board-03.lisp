


(in-package :connect4-test)


;;;
;;;
;;; Status: Review required. Review board and test more detailed.
;;;
;;; Next move by: BLACK
;;;
;;;
(defun create-board-03 ()
  (create-test-board (list
		      "......."
		      "..b..b."
		      ".bww.w."
		      ".wbb.ww"
		      ".bww.ww"
		      ".bbw.bb"
		      )))

;;; Test with traversal depth 6
(define-test test-board-03-a ()
  (let ( (board nil) (best-move nil) (connect4::*classic-skip-randomizer* t))
    (setf board (create-board-03))
    (setf best-move (connect4::minmax board connect4::BLACK 6))
    (assert-equal 0 (first best-move) (format t "test-board-03-a: Wrong move chosen: ~a" (first best-move)))
    ))







