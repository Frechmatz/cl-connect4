

(in-package :connect4-test)

(define-test test-ccfi-row-width-1 ()
  (let ( (width (cfi-server::row-width '("o" "x"))))
    (assert-true (equal 2 width) (format t "test-ccfi-row-width-1 failed"))
    ))

(define-test test-ccfi-row-width-2 ()
  (let ( (width (cfi-server::row-width '("o" "x" 100 "o" "o"))))
    (assert-true (equal 104 width) (format t "test-ccfi-row-width-2 failed"))
    ))

(define-test test-ccfi-row-width-3 ()
  (let ( (width (cfi-server::row-width '(20 "o" "x" 100))))
    (assert-true (equal 122 width) (format t "test-ccfi-row-width-3 failed"))
    ))

;;; Empty row
(define-test test-ccfi-scan-invalid-row-1 ()
  (let ( (got-error nil))
    (handler-case (cfi-server::parse-row "")
      (cfi-server::invalid-placement-error () (setf got-error t)))
    (assert-true got-error (format t "test-ccfi-scan-invalid-row-1 failed"))
    ))

;;; Imvalid characters
(define-test test-ccfi-scan-invalid-row-2 ()
  (let ( (got-error nil))
    (handler-case (cfi-server::parse-row "xabcdo")
      (cfi-server::invalid-placement-error () (setf got-error t)))
    (assert-true got-error (format t "test-ccfi-scan-invalid-row-2 failed"))
    ))

;;; nil board
(define-test test-ccfi-decode-board-1 ()
	     (let ( (got-error nil))
	       (handler-case (cfi-server::decode-placement "" nil nil)
		 (cfi-server::invalid-placement-error () (setf got-error t)))
	       (assert-true got-error (format t "test-ccfi-decode-board-1 failed"))
	       ))

;;; nil board 2
(define-test test-ccfi-decode-board-2 ()
	     (let ( (got-error nil))
	       (handler-case (cfi-server::decode-placement "///" nil nil)
		 (cfi-server::invalid-placement-error () (setf got-error t)))
	       (assert-true got-error (format t "test-ccfi-decode-board-2 failed"))
	       ))

;; simple dimension detection
(define-test test-ccfi-decode-board-3 ()
	     (let ((width nil) (height nil))
	       (cfi-server:decode-placement "xxx/xxx"
				  (lambda (dx dy) (setf width dx) (setf height dy))
				  (lambda (x y token) nil))
	       (assert-true (equal width 3) "test-ccfi-decode-board-3: Failed 1")
	       (assert-true (equal height 2) "test-ccfi-decode-board-3: Failed 2")
	       ))

;; second row longer than first one
(define-test test-ccfi-decode-board-4 ()
	     (let ( (got-error nil))
	       (handler-case
		   (cfi-server::decode-placement "xxx/xxxx"
				       (lambda (dx dy) nil)
				       (lambda (x y token) nil))
		 (cfi-server::invalid-placement-error () (setf got-error t)))
	       (assert-true got-error (format t "test-ccfi-decode-board-4 failed"))
	       ))

;; second row shorter than first one
(define-test test-ccfi-decode-board-5 ()
	     (let ( (got-error nil))
	       (handler-case
		   (cfi-server::decode-placement "xxx/xx"
				       (lambda (dx dy) nil)
				       (lambda (x y token) nil))
		 (cfi-server::invalid-placement-error () (setf got-error t)))
	       (assert-true got-error (format t "test-ccfi-decode-board-5 failed"))
	       ))

(define-test test-ccfi-board-1 ()
	     (let (
		   (expected-board
		    (make-array
		     '(2 3)
		     :initial-contents '(("x" "x" "x") ("o" "o" "o"))))
		   (board nil))
	       (cfi-server::decode-placement
		"xxx/ooo"
		(lambda (dx dy)
		  (setf board (make-array (list dy dx) :initial-element "Q")))
		(lambda (x y token)
		  (setf (aref board y x) token)))
	       (assert-true (equalp expected-board board) (format t "test-ccfi-board-1 failed"))
	       ))


(define-test test-ccfi-board-2 ()
	     (let (
		   (expected-board
		    (make-array
		     '(3 3)
		     :initial-contents '(("x" "x" "x") (nil nil nil) ("o" "o" "o"))))
		   (board nil))
	       (cfi-server::decode-placement
		"xxx/3/ooo"
		(lambda (dx dy)
		  (setf board (make-array (list dy dx) :initial-element "Q")))
		(lambda (x y token)
		  (setf (aref board y x) token)))
	       (assert-true (equalp expected-board board) (format t "test-ccfi-board-2 failed"))
	       ))

(define-test test-ccfi-board-3 ()
	     (let (
		   (expected-board
		    (make-array
		     '(3 4)
		     :initial-contents '((nil "x" "x" "x") (nil nil nil nil) ("o" "o" "o" nil))))
		   (board nil))
	       (cfi-server::decode-placement
		"1xxx/4/ooo1"
		(lambda (dx dy)
		  (setf board (make-array (list dy dx) :initial-element "Q")))
		(lambda (x y token)
		  (setf (aref board y x) token)))
	       (assert-true (equalp expected-board board) (format t "test-ccfi-board-3 failed"))
	       ))

(define-test test-ccfi-board-4 ()
	     (let (
		   (expected-board
		    (make-array
		     '(3 4)
		     :initial-contents '((nil "x" "o" "x") (nil nil nil nil) ("o" "x" "o" nil))))
		   (board nil))
	       (cfi-server::decode-placement
		"1xox/4/oxo1"
		(lambda (dx dy)
		  (setf board (make-array (list dy dx) :initial-element "Q")))
		(lambda (x y token)
		  (setf (aref board y x) token)))
	       (assert-true (equalp expected-board board) (format t "test-ccfi-board-4 failed"))
	       ))
