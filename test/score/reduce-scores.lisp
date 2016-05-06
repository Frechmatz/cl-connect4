
(in-package :connect4-test)

;;; Maximize
(define-test test-get-reduced-scores-1 ()
	     (let (
		   (moves '((0 0 0.0) (1 0 1.0) (2 0 1.0) (3 0 0.0)))
		   (resulting-moves nil)
		   )
	       (setf resulting-moves (score::get-reduced-scores moves nil))
	       (assert-equal 2 (length resulting-moves)
			     (format t "test-get-reduced-scores-1: Wrong number of moves: ~a~%" (length resulting-moves)))
	       (assert-equal 1.0 (third (first resulting-moves))
			     (format t "test-get-reduced-scores-1: Wrong score~%"))
	       (assert-equal 1.0 (third (second resulting-moves))
			     (format t "test-get-reduced-scores-1: Wrong score~%"))
	       (assert-true (or (equal 1 (first (first resulting-moves)))
				(equal 2 (first (first resulting-moves))))
				(format t "test-get-reduced-scores-1: Wrong column~%"))
	       (assert-true (or (equal 1 (first (second resulting-moves)))
				(equal 2 (first (second resulting-moves))))
			    (format t "test-get-reduced-scores-1: Wrong column~%"))
	       (assert-true (not (equal
				  (first (first resulting-moves))
				  (first (second resulting-moves))))
			    (format t "test-get-reduced-scores-1: Columns do not differ~%"))
	       ))

;;; Minimize
(define-test test-get-reduced-scores-2 ()
	     (let (
		   (moves '((0 0 0.0) (1 0 1.0) (2 0 1.0) (3 0 0.0)))
		   (resulting-moves nil)
		   )
	       (setf resulting-moves (score::get-reduced-scores moves t))
	       (assert-equal 2 (length resulting-moves)
			     (format t "test-get-reduced-scores-2: Wrong number of moves: ~a~%" (length resulting-moves)))
	       (assert-equal 0.0 (third (first resulting-moves))
			     (format t "test-get-reduced-scores-2: Wrong score~%"))
	       (assert-equal 0.0 (third (second resulting-moves))
			     (format t "test-get-reduced-scores-2: Wrong score~%"))
	       (assert-true (or (equal 0 (first (first resulting-moves)))
				(equal 3 (first (first resulting-moves))))
			    (format t "test-get-reduced-scores-2: Wrong column~%"))
	       (assert-true (or (equal 0 (first (second resulting-moves)))
				(equal 3 (first (second resulting-moves))))
			    (format t "test-get-reduced-scores-2: Wrong column~%"))
	       (assert-true (not (equal
				  (first (first resulting-moves))
				  (first (second resulting-moves))))
			    (format t "test-get-reduced-scores-2: Columns do not differ~%"))
	       ))

;;; Maximize
(define-test test-reduce-scores-1 ()
	     (let (
		   (moves '((0 0 0.0) (1 0 0.0) (2 0 0.0003) (3 0 0.0)))
		   (move nil)
		   )
	       (setf move (score:reduce-scores moves nil :skip-prefer-center t))
	       (assert-equal 2 (first move) (format t "test-reduce-scores-1: Wrong column chosen: ~a~%" (first move)))
	       ))

;;; Minimize
(define-test test-reduce-scores-2 ()
	     (let (
		   (moves '((0 0 0.0) (1 0 0.0) (2 0 0.0003) (3 0 0.0)))
		   (move nil)
		   )
	       (setf move (score:reduce-scores moves t :skip-randomizer t :skip-prefer-center t))
	       (assert-equal 0 (first move) (format t "test-reduce-scores-2: Wrong column chosen: ~a~%" (first move)))
	       ))

;;; Maximize
(define-test test-reduce-scores-3 ()
	     (let (
		   (moves '((0 0 -5.0) (1 0 -4.0) (2 0 -1.0) (3 0 -4.0)))
		   (move nil)
		   )
	       (setf move (score:reduce-scores moves nil :skip-prefer-center t))
	       (assert-equal 2 (first move) (format t "test-reduce-scores-3: Wrong column chosen: ~a~%" (first move)))
	       ))

;;; Minimize
(define-test test-reduce-scores-4 ()
	     (let (
		   (moves '((0 0 1.0) (1 0 -5.0) (2 0 -4.0) (3 0 0.0)))
		   (move nil)
		   )
	       (setf move (score:reduce-scores moves t :skip-prefer-center t))
	       (assert-equal 1 (first move) (format t "test-reduce-scores-4: Wrong column chosen: ~a~%" (first move)))
	       ))

;;; Maximize (all columns have equal score)
(define-test test-reduce-scores-5 ()
	     (let (
		   (moves '((0 0 0.00) (1 0 0.0) (2 0 0.000) (3 0 0.0000)))
		   (move nil)
		   )
	       (setf move (score:reduce-scores moves nil :skip-randomizer t :skip-prefer-center t))
	       (assert-equal 0 (first move) (format t "test-reduce-scores-5: Wrong column chosen: ~a~%" (first move)))
	       ))

;;; Minimize (all columns have equal score)
(define-test test-reduce-scores-6 ()
	     (let (
		   (moves '((0 0 0.00) (1 0 0.0) (2 0 0.000) (3 0 0.0000)))
		   (move nil)
		   )
	       (setf move (score:reduce-scores moves t :skip-randomizer t :skip-prefer-center t))
	       (assert-equal 0 (first move) (format t "test-reduce-scores-6: Wrong column chosen: ~a~%" (first move)))
	       ))


;;; Empty score list
(define-test test-reduce-scores-7 ()
	     (let (
		   (moves '())
		   (move nil)
		   )
	       (setf move (score:reduce-scores moves t :skip-randomizer t :skip-prefer-center t))
	       (assert-equal nil move (format t "test-reduce-scores-7: Failed~%"))
	       ))

