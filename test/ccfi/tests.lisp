

(in-package :connect4-test)


(define-test test-ccfi-row-width()
  (let ( (width (ccfi::row-width '("o" "x"))))
    (assert-true (equal 2 width) (format t "test-ccfi-row-width failed"))
    ))


