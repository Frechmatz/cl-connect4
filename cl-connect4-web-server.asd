(defsystem :cl-connect4-web-server
  :serial t
  :description "Implements a Connect Four Web Server"
  :long-description "Implements a Connect Four Web Server"
  :depends-on (
	       :hunchentoot
	       :cl-ppcre
	       :alexandria
	       :cl-connect4-cfi-server
	       :cl-who
	       :cl-websocket
	       :cl-svg)
  :components (
	       (:module "src/logger"
			:serial t
			:components ((:file "packages")
				     (:file "logger")
				     ))
	       (:module "src/web-server"
			:serial t
			:components ((:file "packages")
				     (:file "websocket")
				     (:file "query-parser")
				     (:file "server")
				     (:file "board-renderer")
				     (:file "board-renderer-experimental")
				     (:file "buttons")
				     ))
	       ))
