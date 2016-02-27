
(in-package :ccfi)


(alexandria:define-constant TOKEN-X "x" :test #'equal)
(alexandria:define-constant TOKEN-O "o" :test #'equal)

(defun ccfi-token-to-color (token)
  (if (not token)
      connect4-api:EMPTY
      (if (equal token "x") connect4-api:BLACK connect4-api:WHITE)))




