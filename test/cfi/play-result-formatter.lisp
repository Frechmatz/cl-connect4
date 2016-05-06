
(in-package :connect4-test)



(define-test cfi-play-result-formatter-four-1 ()
	     (let ((r
		    (cfi::play-result-formatter-four
		     `(1 1 1 ((2 ,board:WHITE "") (3 ,board:BLACK "") (4 ,board:WHITE "MATE"))))))
	       (assert-true
		(string= r "--four 2/4")
		(format nil "cfi-play-result-formatter-four-1 failed"))
	       ))


(define-test cfi-play-result-formatter-four-n-1 ()
	     (let ((r
		    (cfi::play-result-formatter-four
		     `(5 4 1.e-4 ((5 ,board:WHITE NIL) (2 ,board:BLACK NIL) (5 ,board:WHITE NIL) (5 ,board:BLACK NIL) (5 ,board:WHITE "MATE"))))))
	       (format t "XXXXXXXXXXXXXXXXXXXXXXXXXXX r = ~a" r)
	       (assert-true
		(string= r "--four 5/5/5")
		(format nil "cfi-play-result-formatter-four-n-1 failed"))
	       ))
