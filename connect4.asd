(defsystem :connect4
  :version "0.9.0"
  :licence "Public Domain / 0-clause MIT"
  :description "Implementation of the Connect Four game"
  :long-description
  "Implementation of the Connect 4 game"
  :components
  ((:file "main" :depends-on ("board" "engine" "boardformatter"))
   (:file "board" )
   (:file "engine" :depends-on ("board"))
   (:file "boardformatter" :depends-on ("board"))
   ))
