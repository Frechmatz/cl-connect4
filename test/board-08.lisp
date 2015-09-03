

;;;;
;;;; Status: Approved
;;;;

(in-package :connect4-test)


;;;
;;; Depth: 4 half moves
;;;

(defun create-board-08 ()
  (create-test-board (list
		      "....."
		      "..wW."
		      )))

;;; WHITE must answer with 1.0 for 1
(define-test test-board-08-a ()
	     (let ( (board nil) (best-move nil)
		    (connect4::*classic-skip-randomizer* t)
		    (connect4::*engine-notification-reduced-scores*
		     (lambda (board color is-opponent depth reduced-score all-scores)
		       (declare (ignore board color is-opponent reduced-score))
		       ;; Check final scores
		       (if (equal depth 1)
			   (assert-true (equal-scores-p
					 all-scores
					 '((0 1 0.0) (1 1 1.0) (2 0 0.0) (3 0 0.0) (4 1 0.0))
					 ))
			   ) 
		      ))
		    )
	       (setf board (create-board-08))
	       (setf best-move (connect4::minmax board connect4::WHITE 4))
	       (assert-equal 1 (first best-move)
		(format t "test-board-08-a. Winning situation not detected: Column: ~a~%" (first best-move))
		)
	       ))

;;; BLACK must answer with 0.0 for 0 or 1 or 4 and with -1.0 for 2 and 3 
(define-test test-board-08-b ()
	     (let ( (board nil) (best-move nil)
		    (connect4::*classic-skip-randomizer* t)
		    (connect4::*engine-notification-reduced-scores*
		     (lambda (board color is-opponent depth reduced-score all-scores)
		       (declare (ignore board color is-opponent reduced-score))
		       ;; Check final scores
		       (if (equal depth 1)
			   (assert-true (equal-scores-p
					 all-scores
					 '((0 1 0.0) (1 1 0.0) (2 0 -1.0) (3 0 -1.0) (4 1 0.0))
					 ))
			   ) 
		      ))
		    )
	       (setf board (create-board-08))
	       (setf best-move (connect4::minmax board connect4::BLACK 4))
	       (assert-true (or (equal (first best-move) 0) (equal (first best-move) 1) (equal (first best-move) 4))
		(format t "test-board-08-b. Best move not chosen: Column: ~a~%" (first best-move))
		)
	       ))

