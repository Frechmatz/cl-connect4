



(in-package :connect4-test)

;;;;
;;;; Test calculation of column weights 
;;;;

(define-test test-column-weight-1 ()
	     (let ( (weights (engine::calc-column-weights 4 nil)))
	       ;; (format t "test-column-weight-1: Calculated column weights: ~a~%" weights)
	       (assert-true (equalp (make-array 4 :initial-element 1.0) weights) (format t "test-column-weight-1 failed"))
	       ))

(define-test test-column-weight-2 ()
	     (let ( (weights (engine::calc-column-weights 6 t)))
	       ;;(format t "test-column-weight-2: Calculated column weights: ~a~%" weights)
	       (assert-equal (aref weights 3) 1.0 (format t "test-column-weight-2: Center column does not have weight of 1.0"))
	       (assert-true (< (aref weights 2) (aref weights 3)) (format t "test-column-weight-2: Weight left of center is not smaller than weight of center"))
	       (assert-true (< (aref weights 4) (aref weights 3)) (format t "test-column-weight-2: Weight right of center is not smaller than weight of center"))
	       ))





