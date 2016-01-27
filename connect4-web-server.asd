(defsystem :connect4-web-server
  :serial t
  :description "Implements a Connect Four Web Server"
  :long-description "Implements a Connect Four Web Server"
  :depends-on (:hunchentoot :cl-ppcre :alexandria :connect4 :parenscript)
  :components (
	       (:module "src/web-server"
			:serial t
			:components ((:file "packages")
				     (:file "connect4-js")
				     (:file "server")
				     ))
	       ))



