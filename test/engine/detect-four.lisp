
(in-package :connect4-test)

(define-test test-detect4-diagonal ()
  (let ( (board nil))
    (setf board (create-test-board (list
		      "...bb.."
		      ".bw.w.b"
		      "bb.w.b."
		      "..w.w.."
		      ".w...w."
		      )))
    (assert-true (board:is-four board 2 1) (format t "test-detect4-diagonal-1 failed"))
    (assert-true (board:is-four board 4 1) (format t "test-detect4-diagonal-2 failed"))
    (assert-true (board:is-four board 5 4) (format t "test-detect4-diagonal-3 failed"))
    (assert-true (board:is-four board 1 4) (format t "test-detect4-diagonal-4 failed"))
    (assert-true (board:is-four board 3 2) (format t "test-detect4-diagonal-5 failed"))
    ))

(define-test test-detect4-horizontal ()
  (let ( (board nil))
    (setf board (create-test-board (list
		      "...bb.."
		      ".bbbw.b"
		      "bbwwww."
		      "..wwb.."
		      ".w.b.w."
		      )))
    (assert-true (board:is-four board 2 2) (format t "test-detect4-horizontal-1 failed"))
    (assert-true (board:is-four board 5 2) (format t "test-detect4-horizontal-2 failed"))
    (assert-true (board:is-four board 3 2) (format t "test-detect4-horizontal-3 failed"))
    ))

(define-test test-detect4-vertical ()
  (let ( (board nil))
    (setf board (create-test-board (list
		      ".b..b.."
		      ".bbww.b"
		      "bbbwww."
		      "..wwb.."
		      ".w.w.w."
		      )))
    (assert-true (board:is-four board 3 1) (format t "test-detect4-vertical-1 failed"))
    (assert-true (board:is-four board 3 4) (format t "test-detect4-vertical-2 failed"))
    (assert-true (board:is-four board 3 2) (format t "test-detect4-vertical-3 failed"))
    ))

