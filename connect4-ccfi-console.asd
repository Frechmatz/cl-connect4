(defsystem :connect4-ccfi-console
    :serial t
    :description "Connect Four CCFI Console Implementation"
    :long-description "Connect Four CCFI Console Implementation"
    :depends-on (:cl-ppcre :alexandria :connect4-ccfi)
    :components (
		 (:module "src/ccfi-console"
			  :serial t
			  :components ((:file "packages")
				       (:file "server")
				       ))
		 ))



