
(load "testutil.lisp")

(defun test-line-length-at-1 ()
  (let ((board
	(create-test-board (list
			    "......."
			    "....B.."
			    ".....B."
			    "B....BB"
			    "WbWwWBw"))))

    (format t "a: 3 == ~a~%" (line-length-at board 0 0 0 1 *EMPTY*))
    (format t "b: 3 == ~a~%" (line-length-at board 2 4 1 0 *WHITE*))
    (format t "c: 3 == ~a~%" (line-length-at board 4 1 1 1 *BLACK*))
    (format t "d: 1 == ~a~%" (line-length-at board 0 4 0 1 *WHITE*))
    (format t "e: 7 == ~a~%" (line-length-at board 3 0 1 0 *EMPTY*))
    (format t "f: 1 == ~a~%" (line-length-at board 6 4 1 -1 *WHITE*))
    (format t "g: 0 == ~a~%" (line-length-at board 0 0 1 -1 *WHITE*))
    (format t "h: 1 == ~a~%" (line-length-at board 6 4 1 1 *BLACK*))
    ))

(defun test-max-line-length-at-1 ()
  (let ((board
	(create-test-board (list
			    "......."
			    "....B.."
			    ".....B."
			    "B....BB"
			    "WbWwWBw"))))

    (format t "a: 7 == ~a~%" (max-line-length-at board 0 0 *EMPTY*))
    (format t "b: 3 == ~a~%" (max-line-length-at board 2 4 *WHITE*))
    (format t "c: 3 == ~a~%" (max-line-length-at board 4 1 *BLACK*))
    (format t "d: 1 == ~a~%" (max-line-length-at board 0 4 *WHITE*))
    (format t "e: 7 == ~a~%" (max-line-length-at board 3 0 *EMPTY*))
    (format t "f: 1 == ~a~%" (max-line-length-at board 6 4 *WHITE*))
    (format t "g: 0 == ~a~%" (max-line-length-at board 0 0 *WHITE*))
    (format t "h: 1 == ~a~%" (max-line-length-at board 6 4 *BLACK*))
    ))


(test-line-length-at-1)
(format t "~%~%")
(test-max-line-length-at-1)
