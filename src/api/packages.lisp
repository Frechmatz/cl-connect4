

(defpackage :api
  (:use :cl)
  (:use :board)
  (:use :engine)
  (:export :BLACK)
  (:export :WHITE)
  (:export :MINMAX)
  (:export :GENERATE-MOVES)
  (:export :*ENGINE-NOTIFICATION-REDUCED-SCORES*)
  (:export :DROP)
  (:export :GET-CONNECTED-PIECES)
  (:export :CREATE-BOARD)
  (:export :CLONE-BOARD)
  (:export :GET-FIELD)
  (:export :SET-FIELD)
  (:export :NSET-FIELD)
  (:export :GET-HEIGHT)
  (:export :GET-WIDTH)
  )

