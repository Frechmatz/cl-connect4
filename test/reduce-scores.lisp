
(in-package :connect4-test)

;;;;
;;;; Format of moves: ((x y score) (x y score) ...)
;;;;


;;; Maximize
(define-test test-reduce-scores-1 ()
	     (let (
		   (moves '((0 0 0.0) (1 0 0.0) (2 0 0.0003) (3 0 0.0)))
		   (move nil)
		   )
	       (setf move (connect4::reduce-scores moves nil))
	       (assert-equal 2 (first move) (format t "test-reduce-scores-1: Wrong column chosen: ~a~%" (first move)))
	       ))

;;; Minimize
(define-test test-reduce-scores-2 ()
	     (let (
		   (moves '((0 0 0.0) (1 0 0.0) (2 0 0.0003) (3 0 0.0)))
		   (move nil)
		   )
	       (setf move (connect4::reduce-scores moves t :skip-randomizer t))
	       (assert-equal 0 (first move) (format t "test-reduce-scores-2: Wrong column chosen: ~a~%" (first move)))
	       ))

;;; Maximize
(define-test test-reduce-scores-3 ()
	     (let (
		   (moves '((0 0 -5.0) (1 0 -4.0) (2 0 -1.0) (3 0 -4.0)))
		   (move nil)
		   )
	       (setf move (connect4::reduce-scores moves nil))
	       (assert-equal 2 (first move) (format t "test-reduce-scores-3: Wrong column chosen: ~a~%" (first move)))
	       ))

;;; Minimize
(define-test test-reduce-scores-4 ()
	     (let (
		   (moves '((0 0 1.0) (1 0 -5.0) (2 0 -4.0) (3 0 0.0)))
		   (move nil)
		   )
	       (setf move (connect4::reduce-scores moves t))
	       (assert-equal 1 (first move) (format t "test-reduce-scores-4: Wrong column chosen: ~a~%" (first move)))
	       ))

;;; Maximize (all columns have equal score)
(define-test test-reduce-scores-5 ()
	     (let (
		   (moves '((0 0 0.00) (1 0 0.0) (2 0 0.000) (3 0 0.0000)))
		   (move nil)
		   )
	       (setf move (connect4::reduce-scores moves nil :skip-randomizer t))
	       (assert-equal 0 (first move) (format t "test-reduce-scores-5: Wrong column chosen: ~a~%" (first move)))
	       ))

;;; Minimize (all columns have equal score)
(define-test test-reduce-scores-6 ()
	     (let (
		   (moves '((0 0 0.00) (1 0 0.0) (2 0 0.000) (3 0 0.0000)))
		   (move nil)
		   )
	       (setf move (connect4::reduce-scores moves t :skip-randomizer t))
	       (assert-equal 0 (first move) (format t "test-reduce-scores-6: Wrong column chosen: ~a~%" (first move)))
	       ))


