

(defpackage :ccfi
  (:use :cl :cl-ppcre :alexandria :connect4-api)
  (:export :decode-placement)
  (:export :invalid-placement-error)
  (:export :TOKEN-X)
  (:export :TOKEN-O)
  (:export :ccfi-placement-to-board)
  (:export :ccfi-token-to-color)
  (:export :server)
  (:export :default-server)
  (:export :add-command)
  )

