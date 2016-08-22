

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
	       (assert-true (not (engine:play-result-is-move r)))))

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
	       (assert-true (engine:play-result-is-move r))
	       (assert-true (eql 0 (engine:play-result-row r)))
	       (assert-true (eql 0 (engine:play-result-column r)))))

(define-test play-result-players-color-1 ()
	     (let ((r (engine:play
		       (create-test-board
			(list
			 ".bwbw"
			 "bwbwb"
			 "wbwbw"
			 "bwbwb"))
		       board:WHITE 1)))
	     (assert-true
	      (eql
	       board:WHITE
	       (engine::play-result-players-color r)))))

(define-test play-result-filter-move-sequence-by-token-1 ()
  (let ((result (make-instance 'engine:playresult
			     :column 0
			     :row 0
			     :color board:WHITE
			     :score 0.0
			     :move-sequence `((2 0 ,board:WHITE "") (3 0 ,board:BLACK "") (4 0 ,board:WHITE "")))))
    (let ((r (engine::play-result-filter-move-sequence-by-token result board:BLACK)))
	       (assert-true (eql 1 (length r)))
	       (assert-true (eql (third (first r)) board:BLACK))
	       (assert-true (eql 4 (length (first r)))))))

(define-test play-result-filter-move-sequence-by-token-2 ()
  (let ((result (make-instance 'engine:playresult
			     :column 0
			     :row 0
			     :color board:WHITE
			     :score 0.0
			     :move-sequence `((2 0 ,board:WHITE "") (3 0 ,board:BLACK "") (4 0 ,board:WHITE "")))))
	     (let ((r (engine::play-result-filter-move-sequence-by-token result board:WHITE)))
	       (assert-true (eql 2 (length r)))
	       (assert-true (eql 4 (length (first r))))
	       (assert-true (eql 4 (length (second r))))
	       (assert-true (eql (third (first r)) board:WHITE))
	       (assert-true (eql (third (second r)) board:WHITE)))))

(define-test play-result-players-move-sequence-1 ()
  (let ((result (make-instance 'engine:playresult
			     :column 0
			     :row 0
			     :color board:WHITE
			     :score 0.0
			     :move-sequence `((2 0 ,board:WHITE "") (3 0 ,board:BLACK "") (4 0 ,board:WHITE "")))))
	     (let ((r (engine::play-result-players-move-sequence result)))
	       (assert-true (eql 2 (length r)))
	       (assert-true (eql 2 (first r)))
	       (assert-true (eql 4 (second r))))))

(define-test play-result-is-four-n-1 ()
  (let ((result (make-instance 'engine:playresult
			     :column 0
			     :row 0
			     :color board:WHITE
			     :score 0.0
			     :move-sequence `((2 0 ,board:WHITE "") (3 0 ,board:BLACK "") (4 0 ,board:WHITE "")))))
    (assert-false (engine::play-result-is-four-n result))))

(define-test play-result-is-four-n-2 ()
  (let ((result (make-instance 'engine:playresult
			     :column 0
			     :row 0
			     :color board:WHITE
			     :score 0.0
			     :move-sequence `((2 0 ,board:WHITE "") (3 0 ,board:BLACK 1.0) (4 0 ,board:WHITE "")))))
    (assert-false (engine::play-result-is-four-n result))))

(define-test play-result-is-four-n-3 ()
  (let ((result (make-instance 'engine:playresult
			     :column 0
			     :row 0
			     :color board:WHITE
			     :score 0.0
			     :move-sequence `((2 0 ,board:WHITE 1.0) (3 0 ,board:BLACK "") (4 0 ,board:WHITE "")))))
    (assert-true (engine::play-result-is-four-n result))))

(define-test play-result-is-four-n-4 ()
  (let ((result (make-instance 'engine:playresult
			     :column 0
			     :row 0
			     :color board:WHITE
			     :score 0.0
			     :move-sequence `((2 0 ,board:WHITE "") (3 0 ,board:BLACK "") (4 0 ,board:WHITE 1.0)))))
    (assert-true (engine::play-result-is-four-n result))))

