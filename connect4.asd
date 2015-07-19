(defsystem :connect4
  :version "0.9.0"
  :licence "Public Domain / 0-clause MIT"
  :description "Implementation of the Connect Four game"
  :long-description
  "Implementation of the Connect 4 game"
  :components
  ((:file "main" :depends-on ("board" "boardformatter" "context" "argparser" "command" "commandresult" "classic" "gamecommands"))
   (:file "board" )
   (:file "boardformatter" :depends-on ("board") )
   (:file "context" )
   (:file "argparser" )
   (:file "command" )
   (:file "commandresult" )
   (:file "classic" :depends-on ("board"))
   (:file "gamecommands" :depends-on ("board" "context" "commandresult"))))
