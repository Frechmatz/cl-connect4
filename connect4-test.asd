;;;;
;;;; Unit tests for connect4
;;;;
(defsystem :connect4-test
  :serial t
  :description "Tests of the Connect Four game"
  :long-description "Tests of the Connect Four game"
  :depends-on (:lisp-unit)
  :components (
	       (:module "src/engine"
                        :serial t
                        :components ((:file "packages")
                                     (:file "board")
                                     (:file "engine")))
	       (:module "test/engine"
                        :serial t
                        :components ((:file "packages")
                                     (:file "testutil")
                                     (:file "detect-four")
                                     (:file "board-score")
                                     (:file "column-weights")
                                     (:file "reduce-scores")
                                     (:file "peek-is-four")
				     (:file "no-move-available")
				     (:file "move-available")
                                     (:file "board-00")
                                     (:file "board-01")
                                     (:file "board-02")
				     (:file "board-06")
				     (:file "board-07")
				     (:file "board-08")
				     (:file "board-09")
				     (:file "board-10")
				     ))
	       
  ))



