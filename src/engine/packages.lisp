

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
  (:export :is-field-set)
  (:export :is-four)
  (:export :drop)
  (:export :get-connected-pieces)
  (:export :nset-field)
  (:export :set-field)
  (:export :nclear-field)
  (:export :clear-field)
  (:export :toggle-color)
  )

(defpackage :engine
  (:use :cl)
  (:use :board)
  (:export :CLASSIC-WIDTH)
  (:export :CLASSIC-HEIGHT)
  (:export :MINMAX)
  (:export :IS-MOVE-AVAILABLE)
  (:export :*ENGINE-NOTIFICATION-REDUCED-SCORES*)
  )


