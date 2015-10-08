

;;; Initialize quicklisp (copied from .sbclrc)
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:load-system "connect4-test" :force t)
(in-package :connect4-test)
(format t "~%Running tests...~%")
(use-debugger)
(run-tests)

