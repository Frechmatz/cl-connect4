(defsystem :connect4-web-server
  :serial t
  :description "Implements a Connect Four Web Server"
  :long-description "Implements a Connect Four Web Server"
  :depends-on (:hunchentoot :cl-ppcre :alexandria :connect4-ccfi :parenscript :cl-who :cl-css)
  :components (
	       (:module "src/web-server"
			:serial t
			:components ((:file "packages")
				     (:file "javascript")
				     (:file "server")
				     (:file "css")
				     (:file "board-renderer")
				     ))
	       ))



