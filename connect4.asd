(defsystem :connect4
  :serial t
  :version "0.9.0"
  :licence "Public Domain / 0-clause MIT"
  :description "Implementation of the Connect Four game"
  :long-description
  "Implementation of the Connect 4 game"
  :depends-on (:cl-ppcre)
  :components
  (
   (:file "packages" )
   (:file "board" )
   (:file "boardformatter")
   (:file "engine")
   (:file "connect4")
   ))
