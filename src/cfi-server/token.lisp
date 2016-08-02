
(in-package :cfi-server)


(alexandria:define-constant TOKEN-X "x" :test #'equal)
(alexandria:define-constant TOKEN-O "o" :test #'equal)

(defun ccfi-token-to-color (token)
  (cond
    ((not token)
     board:EMPTY)
    ((equal token "x")
     board:BLACK)
    ((equal token "o")
     board:WHITE)
    (t
     (error 'parse-error :text (format nil "Invalid token: ~a" token)))))




