(defsystem :connect4
  :serial t
  :version "1.0.0"
  :licence "Public Domain / 0-clause MIT"
  :description "Implementation of the Connect Four game"
  :long-description
  "Implementation of the Connect 4 game"
  :depends-on (:cl-ppcre)
  :components (
	       (:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "constants")))
	       (:module "src/engine"
			:serial t
			:components ((:file "packages")
                                     (:file "board")
				     (:file "engine")))
	       (:module "src/console"
			:serial t
			:components ((:file "packages")
				     (:file "context")
				     (:file "parsers")
				     (:file "boardformatter")
				     (:file "messageformatter")
				     (:file "formatters")
				     (:file "formatcontext")
				     (:file "gamecommands")
				     (:file "connect4")
				     ))
   ))
