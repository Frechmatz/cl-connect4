(defsystem :connect4
  :version "0.9.0"
  :licence "Public Domain / 0-clause MIT"
  :description "Implementation of the Connect Four game"
  :long-description
  "Implementation of the Connect 4 game"
  :components
  ((:file "main" :depends-on ("field" "boardformatter" "context" "argparser" "command" "commandresult" "classic" "gamecommands"))
   (:file "field" )
   (:file "boardformatter" :depends-on ("field") )
   (:file "context" )
   (:file "argparser" )
   (:file "command" )
   (:file "commandresult" )
   (:file "classic" :depends-on ("field"))
   (:file "gamecommands" :depends-on ("field" "context" "commandresult"))))
