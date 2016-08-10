
(in-package :connect4-test)


(define-test test-detect4-diagonal ()
  (let ( (board nil))
    (setf board (create-test-board (list
		      "...bb.."
		      ".bw.w.b"
		      "bb.w.b."
		      "..w.w.."
		      ".w...w.")))
    (assert-true (= 4 (length (board:get-connected-pieces board 2 1))))
    (assert-true (= 4 (length (board:get-connected-pieces board 4 1))))
    (assert-true (= 4 (length (board:get-connected-pieces board 5 4))))
    (assert-true (= 4 (length (board:get-connected-pieces board 1 4))))
    (assert-true (= 4 (length (board:get-connected-pieces board 3 2))))))

(define-test test-detect4-horizontal ()
  (let ( (board nil))
    (setf board (create-test-board (list
		      "...bb.."
		      ".bbbw.b"
		      "bbwwww."
		      "..wwb.."
		      ".w.b.w.")))
    (assert-true (= 4 (length (board:get-connected-pieces board 2 2))))
    (assert-true (= 4 (length (board:get-connected-pieces board 5 2))))
    (assert-true (= 4 (length (board:get-connected-pieces board 3 2))))))

(define-test test-detect4-vertical ()
  (let ( (board nil))
    (setf board (create-test-board (list
		      ".b..b.."
		      ".bbww.b"
		      "bbbwww."
		      "..wwb.."
		      ".w.w.w.")))
    (assert-true (= 4 (length (board:get-connected-pieces board 3 1))))
    (assert-true (= 4 (length (board:get-connected-pieces board 3 4))))
    (assert-true (= 4 (length (board:get-connected-pieces board 3 2))))))

