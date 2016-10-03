(defsystem :connect4
  :serial t
  :version "1.0.0"
  :licence "Public Domain / 0-clause MIT"
  :description "Defines and implements a Connect Four API"
  :long-description "Defines and implements a Connect Four API"
  :depends-on (:cl-ppcre :alexandria)
  :components (
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
				     (:file "engine")
				     ))
   ))
