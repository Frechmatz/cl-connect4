



;;;
;;; Test check mate in 6 half moves
;;; WHITE must realize its win
;;;
;;; Status: Ok
;;;

(in-package :connect4-test)


;;;
;;; Depth: 6 half moves
;;; Winning column for WHITE is 1
;;;

(defun create-board-09 ()
  (create-test-board (list
		      "..b.."
		      "w.ww."
		      "w.ww."
		      "b.wb."
		      "b.bw."
		      )))

;;; Check that win situation won't be detected with traversal depth of 4
(define-test test-board-09-a ()
	     (let ( (board nil) (best-move nil)
		    (connect4::*engine-configuration-skip-randomizer* t)
		    (connect4::*engine-configuration-depth-relative-score* nil)
		    (connect4::*engine-notification-reduced-scores*
		     (lambda (board color is-opponent depth reduced-score all-scores)
		       (declare (ignore board color is-opponent reduced-score))
		       ;; Check final scores
		       (if (equal depth 1)
			     (assert-true (equal-scores-p
					   all-scores
					   '((0 0 0.0) (1 4 0.0) (3 0 0.0) (4 4 0.0))
					   )
					  (format t "test-board-09-a: Final score comparison failed")
					  )
			     )
		       ))
		    )
	       (setf board (create-board-09))
	       (setf best-move (connect4::minmax board connect4::WHITE 4))
	       (assert-equal 0 (first best-move)
			     (format t "test-board-09-a. Unexpected move: Column: ~a~%" (first best-move))
			     )
	       ))

;;; Check that win situation is detected with traversal depth of 6
(define-test test-board-09-b ()
	     (let ( (board nil) (best-move nil)
		    (connect4::*engine-configuration-skip-randomizer* t)
		    (connect4::*engine-configuration-depth-relative-score* nil)
		    (connect4::*engine-notification-reduced-scores*
		     (lambda (board color is-opponent depth reduced-score all-scores)
		       (declare (ignore board color is-opponent reduced-score))
		       ;; Check final scores
		       (if (equal depth 1)
			   (assert-true (equal-scores-p
					 all-scores
					 '((0 0 0.0) (1 4 1.0) (3 0 0.0) (4 4 0.0))
			   		 ) (format t "test-board-09-b: Final score comparison failed: ~a" all-scores))
			   ) ; if
		       )
		      )
	       )
	       (setf board (create-board-09))
	       (setf best-move (connect4::minmax board connect4::WHITE 6))
	       (assert-equal 1 (first best-move)
			     (format t "test-board-09-b. Winning situation not detected: Column: ~a~%" (first best-move))
			     )
	       ))

