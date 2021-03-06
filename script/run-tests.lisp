

;;; Initialize quicklisp (copied from .sbclrc)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; Tests do not depend on connect4 system. Just try to compile the stuff as part of testing.
;;(asdf:load-system "connect4" :force t)
;;; Tests do not depend on connect4 system. Just try to compile the stuff as part of testing.
;;(asdf:load-system "connect4-server" :force t)
(asdf:load-system "cl-connect4-test" :force t)
(in-package :connect4-test)
(format t "~%Running tests...~%")
;; uncomment, when tests fail
(setf lisp-unit:*print-failures* t)
(use-debugger)
(run-tests)

