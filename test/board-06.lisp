

(in-package :connect4-test)

;;;
;;; Status: Approved
;;;

(defun create-board-06 ()
  (create-test-board (list
		      "......."
		      "......."
		      ".Bbww.w"
		      )))

(define-test test-board-06-a ()
	     (let ( (board nil) (best-move nil)
		    (connect4::*engine-configuration-skip-randomizer* t)
		    (connect4::*engine-notification-reduced-scores*
		     (lambda (board color is-opponent depth reduced-score all-scores)
		       (if nil
			   (progn
			     (format t
				     "~%Color: ~a Is-Opponent: ~a Depth: ~a Score: ~a~%Final scores:~%~a~%"
				     color is-opponent depth (third reduced-score) all-scores)
			     (connect4::format-board (make-instance 'connect4::board-formatter) board)
			     (format t "~%")
			     )
			   ) ; endif
		       (if (equal depth 1)
			   (assert-true (equal-scores-p
					 all-scores
					 '((0 2 0.0) (1 1 0.0) (2 1 0.0) (3 1 0.0) (4 1 0.0) (5 2 1.0) (6 1 0.0))
					 ))
			   ) ; endif
		      ))
		    )
	       (setf board (create-board-06))
	       (setf best-move (connect4::minmax board connect4::WHITE 1))
	       (assert-true
		(equal 5 (first best-move))
		(format t "test-board-06-a: Wrong move chosen: ~a. Score: ~a~%" (first best-move) (third best-move))
		)
	       ))

(define-test test-board-06-b ()
	     (let ( (board nil) (best-move nil)
		    (connect4::*engine-configuration-skip-randomizer* t)
		    (connect4::*engine-notification-reduced-scores*
		     (lambda (board color is-opponent depth reduced-score all-scores)
		       (if nil
			   (progn
			     (format t
				     "~%Color: ~a Is-Opponent: ~a Depth: ~a Score: ~a~%Final scores:~%~a~%"
				     color is-opponent depth (third reduced-score) all-scores)
			     (connect4::format-board (make-instance 'connect4::board-formatter) board)
			     (format t "~%")
			     )
			   ) ; endif
		       (if (equal depth 1)
			   (assert-true (equal-scores-p
					 all-scores
					 '((0 2 0.0) (1 1 0.0) (2 1 0.0) (3 1 0.0) (4 1 0.0) (5 2 0.0) (6 1 0.0))
					 ))
			   ) ; endif
		      ))
		    )
	       (setf board (create-board-06))
	       (setf best-move (connect4::minmax board connect4::BLACK 1))
	       (assert-true
		(equal 0 (first best-move))
		(format t "test-board-06-b: Wrong move chosen: ~a. Score: ~a~%" (first best-move) (third best-move))
		)
	       ))





