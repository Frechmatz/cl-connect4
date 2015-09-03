;;;;
;;;; Unit tests for connect4
;;;; (require "connect4-test")
;;;; (in-package :connect4-test)
;;;; (run-tests)
;;;;
;;;;
(defsystem :connect4-test
  :serial t
  :description "Tests of the Connect Four game"
  :long-description "Tests of the Connect Four game"
  :depends-on (:connect4 :lisp-unit)
  :components ((:module "test"
                        :serial t
                        :components ((:file "packages")
                                     (:file "testutil")
                                     (:file "board-00")
                                     (:file "board-01")
                                     (:file "board-02")
                                     (:file "board-03")
                                     (:file "board-04")
                                     (:file "board-05")
                                     (:file "board-06")
				     (:file "board-07")
				     (:file "board-08")
                                     (:file "detect-four")
                                     (:file "board-score")
                                     (:file "reduce-scores")
				     )))
  )



