(defsystem :connect4-http-server
  :serial t
  :description "Implements a Connect Four HTTP Server"
  :long-description "Implements a Connect Four HTTP Server"
  :depends-on (:hunchentoot :cl-ppcre :alexandria :connect4)
  :components (
	       (:module "src/http-server"
			:serial t
			:components ((:file "packages")
				     (:file "server")
				     ))
	       ))



