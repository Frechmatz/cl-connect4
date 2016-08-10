

;;;;
;;;; Tests related to the result of the engine::play() function
;;;;

(in-package :connect4-test)


(define-test play-result-board-full ()
	     (let ((r (engine:play
		       (create-test-board
			(list
			 "wbwbw"
			 "bwbwb"
			 "wbwbw"
			 "bwbwb"))
		       board:WHITE 6)))
	       (assert-true r)
	       (assert-true (not (first r)))
	       (assert-true (not (second r)))
	       (assert-true (not (third r)))
	       (assert-true (not (fourth r)))
	       (assert-true (playresult:play-result-no-move-available r))))

(define-test play-result-board-nearly-full ()
	     (let ((r (engine:play
		       (create-test-board
			(list
			 ".bwbw"
			 "bwbwb"
			 "wbwbw"
			 "bwbwb"))
		       board:WHITE 6)))
	       ;;(format t "Result: ~a" r)
	       (assert-true r)
	       (assert-true (eql 4 (length r)))
	       (assert-true (first r))
	       (assert-true (second r))
	       (assert-true (third r))
	       (assert-true (fourth r))))


(define-test play-result-players-color-1 ()
	     (assert-true
	      (eql
	       board:WHITE
	       (playresult::play-result-players-color `(1 1 1 ((2 ,board:WHITE "")))))))

(define-test play-result-filter-move-sequence-by-token-1 ()
	     (let ((r (playresult::play-result-filter-move-sequence-by-token
		     `(1 1 1 ((2 ,board:WHITE "") (3 ,board:BLACK "") (4 ,board:WHITE "")))
		     board:BLACK)))
	       (assert-true (eql 1 (length r)))
	       (assert-true (eql (second (first r)) board:BLACK))
	       (assert-true (eql 3 (length (first r))))))

(define-test play-result-filter-move-sequence-by-token-2 ()
	     (let ((r (playresult::play-result-filter-move-sequence-by-token
		     `(1 1 1 ((2 ,board:WHITE "") (3 ,board:BLACK "") (4 ,board:WHITE "")))
		     board:WHITE)))
	       (assert-true (eql 2 (length r)))
	       (assert-true (eql 3 (length (first r))))
	       (assert-true (eql 3 (length (second r))))
	       (assert-true (eql (second (first r)) board:WHITE))
	       (assert-true (eql (second (second r)) board:WHITE))))

(define-test play-result-players-move-sequence-1 ()
	     (let ((r (playresult::play-result-players-move-sequence
		     `(1 1 1 ((2 ,board:WHITE "") (3 ,board:BLACK "") (4 ,board:WHITE ""))))))
	       (assert-true (eql 2 (length r)))
	       (assert-true (eql 2 (first r)))
	       (assert-true (eql 4 (second r)))))

(define-test play-result-is-four-n-1 ()
	     (assert-false
	      (playresult::play-result-is-four-n
	       `(1 1 1 ((2 ,board:WHITE "") (3 ,board:BLACK "") (4 ,board:WHITE ""))))))

(define-test play-result-is-four-n-2 ()
	     (assert-false
	      (playresult::play-result-is-four-n
	       `(1 1 1 ((2 ,board:WHITE "") (3 ,board:BLACK "MATE") (4 ,board:WHITE ""))))))

(define-test play-result-is-four-n-3 ()
	     (assert-true
	      (playresult::play-result-is-four-n
	       `(1 1 1 ((2 ,board:WHITE "MATE") (3 ,board:BLACK "") (4 ,board:WHITE ""))))))


(define-test play-result-is-four-n-4 ()
	     (assert-true
	      (playresult::play-result-is-four-n
	       `(1 1 1 ((2 ,board:WHITE "") (3 ,board:BLACK "") (4 ,board:WHITE "MATE"))))))

