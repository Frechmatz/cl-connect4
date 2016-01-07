(defsystem :connect4
  :serial t
  :version "1.0.0"
  :licence "Public Domain / 0-clause MIT"
  :description "Implementation of the Connect Four game"
  :long-description
  "Implementation of the Connect 4 game"
  :depends-on (:cl-ppcre)
  :components (
	       (:module "src/board"
			:serial t
			:components ((:file "packages")
                                     (:file "board")))
	       (:module "src/engine"
			:serial t
			:components ((:file "packages")
				     (:file "engine")))
	       (:module "src/ccfi"
			:serial t
			:components ((:file "packages")
				     (:file "decode-placement")
				     ))
	       (:module "src/api"
			:serial t
			:components ((:file "packages")
				     (:file "constants")))
   ))
