
(in-package :ccfi)


(alexandria:define-constant TOKEN-X "x" :test #'equal)
(alexandria:define-constant TOKEN-O "o" :test #'equal)

(defun ccfi-token-to-color (token)
  (if (not token)
      board:EMPTY
      (if (equal token "x") board:BLACK board:WHITE)))




