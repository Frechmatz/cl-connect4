;;;;
;;;; Unit tests for connect4
;;;;
(defsystem :connect4-test
  :serial t
  :description "Tests of the Connect Four game"
  :long-description "Tests of the Connect Four game"
  :depends-on (:lisp-unit :alexandria :cl-ppcre :bordeaux-threads)
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
				     (:file "board-controller")
				     (:file "playresult")
                                     (:file "engine")))
	       (:module "src/cfi-server"
                        :serial t
                        :components ((:file "packages")
				     (:file "token")
				     (:file "play-result-formatter")
                                     (:file "decode-placement")))
	       (:module "test"
                        :serial t
                        :components ((:file "packages")))
	       (:module "test/reduce"
			:serial t
			:components ((:file "reduce-scores")))
	       (:module "test/playresult"
	       		:serial t
	       		:components ((:file "playresult")))
	       (:module "test/engine"
                        :serial t
                        :components ((:file "testutil")
                                     (:file "detect-four")
                                     (:file "board-score")
				     (:file "full-board")
                                     (:file "board-00")
                                     (:file "board-01")
                                     (:file "board-02")
				     (:file "board-06")
				     (:file "board-07")
				     (:file "board-08")
				     (:file "board-09")
				     (:file "board-10")
				     (:file "board-controller")
				     ))
	       (:module "test/cfi-server"
                        :serial t
                        :components ((:file "decode-placement-tests")
				     (:file "play-result-formatter")
				     ))
  ))



