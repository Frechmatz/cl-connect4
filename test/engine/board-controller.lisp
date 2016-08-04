

(in-package :connect4-test)

(define-test test-board-controller-ensure-clone ()
  (let ((board (create-board 5 5)))
    (let ((controller (make-instance 'engine::board-controller :board board)))
      (let ((new-board (engine::set-boardfield controller 4 4 board::WHITE)))
	(assert-true (equal board::EMPTY (get-field board 4 4)) (format t "test-board-controller-ensure-clone failed."))
	(assert-true (equal board::WHITE (get-field new-board 4 4)) (format t "test-board-controller-ensure-clone failed."))
	(assert-true (equal board::WHITE (get-field (engine::get-board controller) 4 4)) (format t "test-board-controller-ensure-clone failed."))
	))))
	

(define-test test-board-controller-1 ()
  (let ((controller (make-instance 'engine::board-controller :board (create-board 6 7))))
    (engine::set-boardfield controller 4 5 board::WHITE)
    (let ((cur-path (engine::get-path controller)) (cur-count (engine::get-count controller)))
      (assert-true (= 1 cur-count) (format t "test-board-controller-1 failed."))
      (assert-true (= 1 (length cur-path)) (format t "test-board-controller-1 failed."))
      (assert-true (equal `( (4 5 ,board::WHITE)) cur-path)
		   (format t "test-board-controller-1 failed. ~a" cur-path)))))
  

(define-test test-board-controller-2 ()
  (let ((controller (make-instance 'engine::board-controller :board (create-board 6 7))))
    (engine::set-boardfield controller 4 5 board::WHITE)
    (engine::set-boardfield controller 5 5 board::BLACK)
    (let ((cur-path (engine::get-path controller)) (cur-count (engine::get-count controller)))
      (assert-true (= 2 cur-count) (format t "test-board-controller-2 failed."))
      (assert-true (= 2 (length cur-path)) (format t "test-board-controller-2 failed."))
      (assert-true (equal `( (5 5 ,board::BLACK) (4 5 ,board::WHITE)) cur-path)
		   (format t "test-board-controller-2 failed. ~a" cur-path)))))

(define-test test-board-controller-3 ()
  (let ((controller (make-instance 'engine::board-controller :board (create-board 6 7))))
    (engine::set-boardfield controller 4 5 board::WHITE)
    (engine::set-boardfield controller 5 5 board::BLACK)
    (engine::undo-set-boardfield controller)
    (let ((cur-path (engine::get-path controller)) (cur-count (engine::get-count controller)))
      (assert-true (= 2 cur-count) (format t "test-board-controller-3 failed."))
      (assert-true (= 1 (length cur-path)) (format t "test-board-controller-3 failed."))
      (assert-true (equal `( (4 5 ,board::WHITE)) cur-path)
		   (format t "test-board-controller-3 failed. ~a" cur-path)))))

