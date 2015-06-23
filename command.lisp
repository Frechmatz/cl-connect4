

#|
Defines a command that can be executed by the REPL
|#


(defclass command ()
  (
   (name :initarg :name)
   (infoFn :initarg :infoFn)
   (parseArgsFn :initarg :parseArgsFn)
   (descriptionFn :initarg :descriptionFn)
   (execFn :initarg :execFn)
   (tags :initarg :tags)
   ))

