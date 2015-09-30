


;;;;
;;;; Some basic 3 pieces in a row related tests  
;;;;

(in-package :connect4-test)

;;; Basic test not prefering the center of the board
(define-test test-board-10-a ()
	     (run-minmax-test 
	      "test-board-10-a"
	      (create-test-board (list
		      "....."
		      "....."
		      "....."
		      "....."
		      ))
	      connect4::WHITE 6
	      ;; :print-final-scores t
	      :expected-final-columns '(0 1 2 3 4)
	      ;;:expected-final-scores '((0 3 0.0) (1 3 0.0) (2 3 0.0) (3 3 0.0) (4 3 0.0))
	      ;; :print-engine-configuration t
	      :engine-configuration-prefer-center nil
	      ))

;;; Basic test prefering the center of the board (1 half-move)
(define-test test-board-10-b ()
	     (run-minmax-test 
	      "test-board-10-b"
	      (create-test-board (list
		      "....."
		      "....."
		      "....."
		      "....."
		      ))
	      connect4::WHITE 1
	      :print-final-scores t
	      :expected-final-columns '(2)
	      ;;:expected-final-scores '((0 3 0.0) (1 3 0.0) (2 3 0.0) (3 3 0.0) (4 3 0.0))
	      :print-engine-configuration t
	      :engine-configuration-prefer-center t
	      ))

#|
;;; Basic test prefering the center of the board (2 half-moves)
(define-test test-board-10-c ()
	     (run-minmax-test 
	      "test-board-10-c"
	      (create-test-board (list
		      "....."
		      "....."
		      "....."
		      "....."
		      ))
	      connect4::WHITE 2
	      :print-final-scores t
	      ;;:print-all-scores t
	      :expected-final-columns '(2)
	      ;;:expected-final-scores '((0 3 0.0) (1 3 0.0) (2 3 0.0) (3 3 0.0) (4 3 0.0))
	      :print-engine-configuration t
	      :engine-configuration-prefer-center nil
	      ))
|#


;;;
;;; WHITE: Realize three pieces in a row via 3 
;;;
(define-test test-board-10-d ()
	     (run-minmax-test 
	      "test-board-10-d"
	      (create-test-board (list
		      "....."
		      "....."
		      "...w."
		      "...w."
		      ))
	      connect4::WHITE 1
	      ;; :print-final-scores t
	      :expected-final-columns '(3)
	      ;;:expected-final-scores '((0 3 0.0) (1 3 0.0) (2 3 0.0) (3 1 0.5) (4 3 0.0))
	      ))

;;;
;;; BLACK: Hold off three pieces in a row for WHITE via 3 
;;;
(define-test test-board-10-e ()
	     (run-minmax-test 
	      "test-board-10-e"
	      (create-test-board (list
		      "....."
		      "....."
		      "...w."
		      "...w."
		      ))
	      connect4::BLACK 2
	      ;; :print-final-scores t
	      :expected-final-columns '(3)
	      ;;:expected-final-scores '((0 3 -0.05) (1 3 -0.05) (2 3 -0.05) (3 1 0.0) (4 3 -0.05))
	      ))

