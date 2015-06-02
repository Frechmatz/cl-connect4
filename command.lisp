

#|
Defines a command that can be executed by the REPL
|#


(defclass command ()
  (
   (infoFn :initarg :infoFn)
   (descriptionFn :initarg :descriptionFn)
   (execFn :initarg :execFn)
   ))

