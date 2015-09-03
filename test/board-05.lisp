

(in-package :connect4-test)

;;;
;;; Next move by: BLACK
;;; 
;;; Computer must throw into column 2 otherwise
;;; WHITE will win via 3 - 5 - 5
;;;
;;; In diesem Test muß Schwarz den nächsten Zug machen.
;;; dieser Test soll testen, das Schwarz verliert, wenn es nicht in Spalte 3 wirft
;;; Prüfungen müssen sein, das wenn weiss in 3 einwirft, schwarz verloren hat. Bei
;;; Einwurf von weiss in spalte 3 müssen die scores negativ sein.
;;; die antwort von schwarz muss deshalb 3 sein.
;;; in dieser Stellung kann schwarz auch mit 1 antworten, um eine direkte bedrohung
;;; aufzubauen, die abgewehrt werden muss.
;;;
(defun create-board-05 ()
  (create-test-board (list
		      "....B..."
		      "....w.w."
		      ".B.ww.w."
		      "wB.bw.bb"
		      )))

;;; Test with traversal depth 6 BLACK
(define-test test-board-05-a ()
	     (let ( (board nil) (best-move nil)
		    (connect4::*classic-skip-randomizer* t)
		    (connect4::*engine-notification-reduced-scores*
		     (lambda (board color is-opponent depth reduced-score all-scores)
		       ;; gegner (W) hat in drei geworfen und Computer verliert. Scores müssen negativ sein.
		       (if (and (equal color connect4::BLACK) (connect4::is-field-color-white-p board 3 1) (>= 0 (third reduced-score)))
			   (progn
			     (format t
				     "~%Weiss hat tödlichen Zug in 3 gemacht. Verlust vom Computer (B) nicht erkannt.~%Color: ~a Is-Opponent: ~a Depth: ~a Score: ~a~%Final scores:~%~a~%"
				     color is-opponent depth (third reduced-score) all-scores)
			     (connect4::format-board (make-instance 'connect4::board-formatter) board)
			     (format t "~%")
			     )
			   ) ;; /end if
		       )
		      )
		    )
	       (setf board (create-board-05))
	       (setf best-move (connect4::minmax board connect4::BLACK 6))
	       (assert-true
		(or
		 (equal 1 (first best-move))
		 (equal 3 (first best-move)))
		(format t "test-board-05-a: Wrong move chosen: ~a. Score: ~a~%" (first best-move) (third best-move))
		)
	       ))

