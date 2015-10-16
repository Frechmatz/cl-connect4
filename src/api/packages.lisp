

(defpackage :api
  (:use :cl)
  (:use :board)
  (:use :engine)
  (:export :MINMAX)
  (:export :GENERATE-MOVES)
  (:export :*ENGINE-NOTIFICATION-REDUCED-SCORES*)
  (:export :BLACK)
  (:export :WHITE)
  (:export :CREATE-BOARD)
  (:export :DROP)
  (:export :GET-CONNECTED-PIECES)
  (:export :GET-FIELD)
  (:export :GET-HEIGHT)
  (:export :GET-WIDTH)
  (:export :IS-FOUR)
  (:export :SET-FIELD)
  )


