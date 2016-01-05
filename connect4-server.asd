
;;;;
;;;; Connect Four Server
;;;;
(defsystem :connect4-server
  :serial t
  :description "Connect Four Server"
  :long-description "A Connect Four Server"
  :depends-on (:hunchentoot :cl-ppcre)
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
			     (:file "constants")
			     (:file "encode-position")
			     (:file "decode-position")
			     (:file "server")
			     ))
	       (:module "src/server"
                        :serial t
		:components ((:file "packages")
			     (:file "server")
			     ))
	       
	       ))



