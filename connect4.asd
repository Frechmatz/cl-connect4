(defsystem :connect4
  :serial t
  :version "1.0.0"
  :licence "Public Domain / 0-clause MIT"
  :description "Defines and implementation a Connect Four API"
  :long-description "Defines and implementation a Connect Four API"
  :depends-on (:cl-ppcre :alexandria)
  :components (
	       (:module "src/connect4/board"
			:serial t
			:components ((:file "packages")
                                     (:file "board")))
	       (:module "src/connect4/engine"
			:serial t
			:components ((:file "packages")
				     (:file "engine")))
	       (:module "src/connect4/api"
			:serial t
			:components ((:file "packages")
				     (:file "constants")))
   ))
