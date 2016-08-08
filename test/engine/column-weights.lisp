



(in-package :connect4-test)

;;;;
;;;; Test calculation of column weights 
;;;;

(define-test test-column-weight-2 ()
	     (let ( (weights (engine::calc-column-weights 6)))
	       ;;(format t "test-column-weight-2: Calculated column weights: ~a~%" weights)
	       (assert-equal (aref weights 3) 1.0)
	       (assert-true (< (aref weights 2) (aref weights 3)))
	       (assert-true (< (aref weights 4) (aref weights 3)))))





