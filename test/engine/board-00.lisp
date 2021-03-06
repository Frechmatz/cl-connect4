

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
	     (let ((result
		    (engine:play
		     (create-board-00)
		     board:WHITE
		     1)))
	       (format t "Sequence: ~a~%" (engine::play-result-move-sequence result))
	       (assert-played-column result '(2))
	       (assert-is-mate result)
	       ))

;;; Test with traversal depth 6
;;; Computer must chose direct win
(define-test test-board-00-c ()
	     (let ((result
		    (engine:play
		     (create-board-00)
		     board:WHITE
		     6)))
	       (format t "board-00-c Column: ~a~%" (play-result-column result))
	       (assert-played-column result '(2))
	       (assert-is-mate result)))


