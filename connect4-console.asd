(defsystem :connect4-console
  :serial t
  :version "1.0.0"
  :licence "Public Domain / 0-clause MIT"
  :description "Connect Four console interface"
  :long-description
  "Connect Four console interface"
  :depends-on (:cl-ppcre :alexandria :connect4)
  :components (
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
