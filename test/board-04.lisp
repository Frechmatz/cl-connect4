
(in-package :connect4-test)

;;;
;;;
;;; Status: Review required. Not sure what the test tests at all. Not part of test suite.
;;;
;;; Next move by: BLACK
;;; 
;;; Computer must throw into column 2 otherwise
;;; WHITE will win via 2 - 4 - 4
;;;
(defun create-board-04 ()
  (create-test-board (list
		      "......."
		      "......."
		      "...B..."
		      "...w.w."
		      "B.ww.w."
		      "B.bw.bb"
		      )))

;;; Test with traversal depth 6 BLACK
(define-test test-board-04-a ()
  (let ( (board nil) (best-move nil) (connect4::*engine-configuration-skip-randomizer* t))
    (setf board (create-board-04))
    (setf best-move (connect4::minmax board connect4::BLACK 6))
    (assert-equal 2 (first best-move) (format t "test-board-04-a: Wrong move chosen: ~a. Score: ~a~%" (first best-move) (third best-move)))
    ))

;;; Test with traversal depth 6 WHITE
(define-test test-board-04-b ()
  (let ( (board nil) (best-move nil) (connect4::*engine-configuration-skip-randomizer* t))
    (setf board (create-board-04))
    (setf best-move (connect4::minmax board connect4::WHITE 6))
    (assert-equal 2 (first best-move) (format t "test-board-04-b: Wrong move chosen: ~a. Score: ~a~%" (first best-move) (third best-move)))
    ))

