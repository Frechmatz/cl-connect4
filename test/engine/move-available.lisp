

(in-package :connect4-test)

(define-test test-move-available-1 ()
  (let ( (board nil))
    (setf board (create-test-board (list
		      "......."
		      "......."
		      "......."
		      "......."
		      )))
    (assert-true (engine::is-move-available board) (format t "test-move-available-1 failed"))
    ))

(define-test test-move-available-2 ()
  (let ( (board nil))
    (setf board (create-test-board (list
		      "w......"
		      "w......"
		      "w......"
		      "w......"
		      )))
    (assert-true (engine::is-move-available board) (format t "test-move-available-2 failed"))
    ))

(define-test test-move-available-3 ()
  (let ( (board nil))
    (setf board (create-test-board (list
		      "w.....w"
		      "w.....w"
		      "w.....w"
		      "w.....w"
		      )))
    (assert-true (engine::is-move-available board) (format t "test-move-available-3 failed"))
    ))

(define-test test-move-available-4 ()
  (let ( (board nil) )
    (setf board (create-test-board (list
		      "..w...."
		      "..w...."
		      "..w...."
		      "..w...."
		      )))
    (assert-true (engine::is-move-available board) (format t "test-move-available-4 failed"))
    ))

(define-test test-move-available-5 ()
  (let ( (board nil))
    (setf board (create-test-board (list
		      "www.www"
		      "wwwwwww"
		      )))
    (assert-true (engine::is-move-available board) (format t "test-move-available-5 failed"))
    ))

(define-test test-move-available-6 ()
  (let ( (board nil) )
    (setf board (create-test-board (list
		      "wwwwwww"
		      "wwwwwww"
		      )))
    (assert-false (engine::is-move-available board) (format t "test-move-available-6 failed"))
    ))