

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

;;; Empty row
(define-test test-ccfi-scan-invalid-row-1 ()
  (let ( (got-error nil))
    (handler-case (ccfi::scan-row "")
      (ccfi::invalid-field-definition-error () (setf got-error t)))
    (assert-true got-error (format t "test-ccfi-scan-invalid-row-1 failed"))
    ))

;;; Imvalid characters
(define-test test-ccfi-scan-invalid-row-2 ()
  (let ( (got-error nil))
    (handler-case (ccfi::scan-row "xabcdo")
      (ccfi::invalid-field-definition-error () (setf got-error t)))
    (assert-true got-error (format t "test-ccfi-scan-invalid-row-2 failed"))
    ))

;;; nil board
(define-test test-ccfi-decode-board-1 ()
	     (let ( (got-error nil))
	       (handler-case (ccfi::decode-board "" nil nil)
		 (ccfi::invalid-field-definition-error () (setf got-error t)))
	       (assert-true got-error (format t "test-ccfi-decode-board-1 failed"))
	       ))

;;; nil board 2
(define-test test-ccfi-decode-board-2 ()
	     (let ( (got-error nil))
	       (handler-case (ccfi::decode-board "///" nil nil)
		 (ccfi::invalid-field-definition-error () (setf got-error t)))
	       (assert-true got-error (format t "test-ccfi-decode-board-2 failed"))
	       ))

;; simple dimension detection
(define-test test-ccfi-decode-board-3 ()
	     (let ((width nil) (height nil))
	       (ccfi:decode-board "xxx/xxx"
				  (lambda (dx dy) (setf width dx) (setf height dy))
				  (lambda (x y token) nil))
	       (assert-true (equal width 3) "test-ccfi-decode-board-3: Failed 1")
	       (assert-true (equal height 2) "test-ccfi-decode-board-3: Failed 2")
	       ))

;; second row longer than first one
(define-test test-ccfi-decode-board-4 ()
	     (let ( (got-error nil))
	       (handler-case
		   (ccfi::decode-board "xxx/xxxx"
				       (lambda (dx dy) nil)
				       (lambda (x y token) nil))
		 (ccfi::invalid-field-definition-error () (setf got-error t)))
	       (assert-true got-error (format t "test-ccfi-decode-board-4 failed"))
	       ))

#|
;; second row shorter than first one
(define-test test-ccfi-decode-board-5 ()
	     (let ( (got-error nil))
	       (handler-case
		   (ccfi::decode-board "xxx/xx"
				       (lambda (dx dy) nil)
				       (lambda (x y token) nil))
		 (ccfi::invalid-field-definition-error () (setf got-error t)))
	       (assert-true got-error (format t "test-ccfi-decode-board-5 failed"))
	       ))

|#

	     
	     
