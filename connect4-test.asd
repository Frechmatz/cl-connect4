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
                                     (:file "minmaxtest")
                                     (:file "board-01")
				     )))
  )



