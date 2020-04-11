(defsystem :cl-connect4
  :serial t
  :version "1.0.0"
  :licence "Public Domain / 0-clause MIT"
  :description "Defines and implements a Connect Four API"
  :long-description "Defines and implements a Connect Four API"
  :depends-on (:cl-ppcre
	       :alexandria
	       :cl-svg
	       :bordeaux-threads
	       :queues.simple-cqueue
	       :cl-websocket
	       :hunchentoot
	       :cl-who)
  :components (
	       (:module "src/logger"
			:serial t
			:components ((:file "packages")
				     (:file "logger")))
	       (:module "src/connect4/board"
			:serial t
			:components ((:file "packages")
                                     (:file "board")))
 	       (:module "src/connect4/reduce"
			:serial t
			:components ((:file "packages")
                                     (:file "reduce")))
	       (:module "src/connect4/movegenerator"
			:serial t
			:components ((:file "packages")
                                     (:file "movegenerator")))
	       (:module "src/connect4/engine"
			:serial t
			:components ((:file "packages")
				     (:file "playresult")
				     (:file "board-controller")
				     (:file "board-variance")
				     (:file "engine")))
	       (:module "src/cfi-server"
			:serial t
			:components ((:file "packages")
				     (:file "token")
				     (:file "decode-placement")
				     (:file "formatter-util")
				     (:file "play-result-formatter")
				     (:file "info-formatter")
				     (:file "encode-placement")
				     (:file "lazy-handler")
				     (:file "server-class")
				     (:file "server-commands")
				     (:file "server")))
	       
	       (:module "src/web-server"
			:serial t
			:components ((:file "packages")
				     (:file "websocket")
				     (:file "query-parser")
				     (:file "server")
				     (:file "board-renderer")
				     (:file "board-renderer-experimental")
				     (:file "buttons")))
	       (:module "src/cfi-console"
			:serial t
			:components ((:file "packages")
				     (:file "connect4")))

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
				     (:file "connect4")))
	       
	       
   ))
