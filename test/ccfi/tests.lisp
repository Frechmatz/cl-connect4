

(in-package :connect4-test)


(define-test test-ccfi-row-width-1 ()
  (let ( (width (ccfi::row-width '("o" "x"))))
    (assert-true (equal 2 width) (format t "test-ccfi-row-width-1 failed"))
    ))

(define-test test-ccfi-row-width-2 ()
  (let ( (width (ccfi::row-width '("o" "x" 100 "o" "o"))))
    (assert-true (equal 104 width) (format t "test-ccfi-row-width-2 failed"))
    ))

(define-test test-ccfi-row-width-3 ()
  (let ( (width (ccfi::row-width '(20 "o" "x" 100))))
    (assert-true (equal 122 width) (format t "test-ccfi-row-width-3 failed"))
    ))

(define-test test-ccfi-scan-invalid-row-1 ()
  (let ( (got-error nil))
    (handler-case (ccfi::scan-row "")
      (ccfi::invalid-field-definition-error () (setf got-error t)))
    (assert-true got-error (format t "test-ccfi-scan-invalid-row-1 failed"))
    ))

(define-test test-ccfi-scan-invalid-row-2 ()
  (let ( (got-error nil))
    (handler-case (ccfi::scan-row "xabcdo")
      (ccfi::invalid-field-definition-error () (setf got-error t)))
    (assert-true got-error (format t "test-ccfi-scan-invalid-row-2 failed"))
    ))
