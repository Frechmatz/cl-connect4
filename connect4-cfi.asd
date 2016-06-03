(defsystem :connect4-cfi
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
	       (:module "src/cfi"
			:serial t
			:components ((:file "packages")
				     (:file "token")
				     (:file "play-result-formatter")
				     (:file "server")
;;				     (:file "default-server")
				     (:file "decode-placement")
				     ))
   ))
