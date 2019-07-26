(defsystem :cl-connect4-cfi-console
  :serial t
  :version "1.0.0"
  :licence "Public Domain / 0-clause MIT"
  :depends-on (:cl-ppcre :alexandria :cl-connect4-cfi-server)
  :components (
	       (:module "src/cfi-console"
			:serial t
			:components ((:file "packages")
				     (:file "connect4")
				     ))
   ))
