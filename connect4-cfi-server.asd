(defsystem :connect4-cfi-server
  :serial t
  :version "1.0.0"
  :licence "Public Domain / 0-clause MIT"
  :description "Implements the Connect Four Interface (CFI)"
  :long-description "Implements the Connect Four Interface (CFI)"
  :depends-on (:cl-ppcre :alexandria :connect4 :queues.simple-cqueue)
  :components (
	       (:module "src/logger"
			:serial t
			:components ((:file "packages")
				     (:file "logger")
				     ))
	       (:module "src/cfi-server"
			:serial t
			:components ((:file "packages")
				     (:file "token")
				     (:file "decode-placement")
				     (:file "play-result-formatter")
				     (:file "info-formatter")
				     (:file "encode-placement")
				     (:file "lazy-handler")
				     (:file "server-class")
				     (:file "server-commands")
				     (:file "server")
				     ))
   ))
