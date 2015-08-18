
(in-package :connect4-test)

;;;
;;; Next move by: WHITE (the computer)
;;; Direct win 
;;;
(define-test test-direct-win ()
  (let ( (board nil) (best-move nil) (connect4::*classic-skip-randomizer* t))
    (setf board (create-test-board (list
				    ".."
				    ".w"
				    ".w"
				    "bw")))
    (setf best-move (connect4::minmax board connect4::*WHITE* 3))
    (assert-equal 1 (first best-move) (format t "test-direct-win: Wrong move chosen: ~a" (first best-move)))
    ))


;;;
;;; Next move by: WHITE (the computer)
;;; 'checkmate' in two moves
;;;
(define-test test-win-by-two ()
  (let ( (board nil) (best-move nil) (connect4::*classic-skip-randomizer* t))
    (setf board (create-test-board (list
				    "wb...."
				    "bbww.w"
				    "bbww.w"
				    "bwbb..")))
    (setf best-move (connect4::minmax board connect4::*WHITE* 4))
    (assert-equal 4 (first best-move) (format t "test-win-by-two: Wrong move chosen: ~a" (first best-move)))
    ))

