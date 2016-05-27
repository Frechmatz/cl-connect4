(defsystem :connect4-web-server
  :serial t
  :description "Implements a Connect Four Web Server"
  :long-description "Implements a Connect Four Web Server"
  :depends-on (
	       :hunchentoot
	       :cl-ppcre
	       :alexandria
	       :connect4-cfi
	       :cl-who
	       :hunchensocket
	       ;;:defrest
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
				     (:file "server")
				     (:file "board-renderer")
				     (:file "board-renderer-experimental")
				     (:file "buttons")
				     ))
	       ))
