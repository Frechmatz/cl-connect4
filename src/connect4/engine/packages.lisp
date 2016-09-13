

(defpackage :engine
  (:use :cl)
  (:use :board)
  (:use :reduce)
  (:use :movegenerator)
  (:export :play)
  (:export :playresult)
  (:export :play-result-players-color)
  (:export :play-result-column)
  (:export :play-result-row)
  (:export :play-result-score)
  (:export :play-result-is-move)
  (:export :play-result-is-four-1)
  (:export :play-result-is-four-n)
  (:export :play-result-final-scores)
  (:export :play-result-players-move-sequence))


