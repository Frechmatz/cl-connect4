

(defpackage :board
  (:use :cl)
  (:export :BLACK)
  (:export :WHITE)
  (:export :EMPTY)
  (:export :create-board)
  (:export :clone-board)
  (:export :get-height)
  (:export :get-width)
  (:export :get-field)
  (:export :field-set-p)
  (:export :drop)
  (:export :get-connected-pieces)
  (:export :nset-field)
  (:export :set-field)
  (:export :nclear-field)
  (:export :clear-field)
  )

