

(defpackage :cfi
  (:use :cl :cl-ppcre :alexandria :board :engine)
  (:export :decode-placement)
  (:export :invalid-placement-error)
  (:export :TOKEN-X)
  (:export :TOKEN-O)
  (:export :ccfi-placement-to-board)
  (:export :ccfi-token-to-color)
  (:export :cfi-server)
  (:export :put-command)
  (:export :start)
  (:export :stop)
  )

