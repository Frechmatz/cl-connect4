(defsystem :connect4-ccfi
  :serial t
  :version "1.0.0"
  :licence "Public Domain / 0-clause MIT"
  :description "Implements the Common Connect Four Interface (CCFI)"
  :long-description "Implements the Common Connect Four Interface (CCFI)"
  :depends-on (:cl-ppcre :alexandria :connect4)
  :components (
	       (:module "src/logger"
			:serial t
			:components ((:file "packages")
				     (:file "logger")
				     ))
	       (:module "src/ccfi"
			:serial t
			:components ((:file "packages")
				     (:file "token")
				     (:file "server")
				     (:file "default-server")
				     (:file "decode-placement")
				     ))
   ))
