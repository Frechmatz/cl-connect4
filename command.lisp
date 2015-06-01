

#|
Defines a command that can be executed by the REPL
|#


(defclass command ()
  (
   (infoFn :initarg :infoFn)
   (execFn :initarg :execFn)
   ))

