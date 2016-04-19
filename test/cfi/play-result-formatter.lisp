
(in-package :connect4-test)



(define-test ccfi-play-result-formatter-four-1 ()
	     (let ((r
		    (ccfi::play-result-formatter-four
		     `(1 1 1 ((2 ,board:WHITE "") (3 ,board:BLACK "") (4 ,board:WHITE "MATE"))))))
	       (format t "r = ~a" r)
	       (assert-true
		(string= r "--four 2/4")
		(format nil "ccfi-play-result-formatter-four-1 failed"))
	       ))


