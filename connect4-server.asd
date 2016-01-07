
;;;;
;;;; Connect Four Server
;;;;
(defsystem :connect4-server
    :serial t
    :description "Connect Four Server"
    :long-description "A Connect Four Server"
    :depends-on (:hunchentoot :cl-ppcre :connect4)
    :components (
		 (:module "src/server"
			  :serial t
			  :components ((:file "packages")
				       (:file "server")
				       ))
		 (:module "src/ccfi-server"
			  :serial t
			  :components ((:file "packages")
				       (:file "server")
				       ))
		 ))



