
(in-package :connect4-test)

(define-test cfi-play-result-formatter-four-1 ()
  (let ((result (make-instance 'engine:playresult
			     :column 0
			     :row 0
			     :color board:WHITE
			     :score 0.0
			     :move-sequence `((2 0 ,board:WHITE nil) (3 0 ,board:BLACK nil) (4 0 ,board:WHITE 1.0)))))
	     (let ((r (cfi-server::play-result-formatter-four result)))
	       (assert-true
		(string= r "--four 2/4")
		(format t "cfi-play-result-formatter-four-1 failed"))
	       )))


(define-test cfi-play-result-formatter-four-n-1 ()
  (let ((result (make-instance 'engine:playresult
			     :column 0
			     :row 0
			     :color board:WHITE
			     :score 0.0
			     :move-sequence
			     `((5 0 ,board:WHITE NIL)
				  (2 0 ,board:BLACK NIL)
				  (5 0 ,board:WHITE NIL)
				  (5 0 ,board:BLACK NIL)
				  (5 0 ,board:WHITE 1.0))
			     )))
    (let ((r  (cfi-server::play-result-formatter-four result)))
      (assert-true
		(string= r "--four 5/5/5")
		(format t "cfi-play-result-formatter-four-n-1 failed"))
      )))
