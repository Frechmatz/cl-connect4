
(in-package :connect4-test)


;;;
;;;
;;; Status: Approved
;;;
;;; Depth: 2 half moves
;;;

(defun create-board-07 ()
  (create-test-board (list
		      "....."
		      ".wWw."
		      )))

;;; BLACK must answer with negative value for all columns
(define-test test-board-07-a ()
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
					 '((0 1 -1.0) (1 0 -1.0) (2 0 -1.0) (3 0 -1.0) (4 1 -1.0))
					 ) "test-board-07-a: Final score comparison failed")
			   ) 
		      ))
		    )
	       (setf board (create-board-07))
	       (setf best-move (connect4::minmax board connect4::BLACK 2))
	       (assert-true (equal (third best-move) -1.0)
		(format t "test-board-07-a. Losing situation not detected: Score: ~a~%" (third best-move))
		)
	       ))

;;; WHITE must answer with 0 or 4
(define-test test-board-07-b ()
	     (let ( (board nil) (best-move nil)
		    (connect4::*engine-configuration-skip-randomizer* t)
		    (connect4::*engine-notification-reduced-scores*
		     (lambda (board color is-opponent depth reduced-score all-scores)
		       (declare (ignore board color is-opponent reduced-score))
		       ;; Check final scores
		       (if (equal depth 1)
			   (assert-true (equal-scores-p
					 all-scores
					 '((0 1 1.0) (1 0 0.0) (2 0 0.0) (3 0 0.0) (4 1 1.0))
					 ) "test-board-07-b: Final score comparison failed")
			   ) 
		      ))
		    )
	       (setf board (create-board-07))
	       (setf best-move (connect4::minmax board connect4::WHITE 2))
	       (assert-true (or (equal (first best-move) 0) (equal (first best-move) 4))
			    (format t "test-board-07-b. Win situation not detected: Column: ~a~%" (first best-move))
		)
	       ))
