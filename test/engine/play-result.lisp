

;;;;
;;;; Tests related to the result of the engine::play() function
;;;;

(in-package :connect4-test)


(define-test play-result-board-full ()
	     (let ((r (run-minmax-test 
	      "play-result-board-full"
	      (create-test-board (list
		      "wbwbw"
		      "bwbwb"
		      "wbwbw"
		      "bwbwb"
		      ))
	      board:WHITE 6)))
	       (assert-true r (format nil "Result not set"))))






