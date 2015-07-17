

(defclass command-result ()
  (
   (redraw-board :initarg :redraw-board)
   (message :initarg :message)
   (highlight-cells :initarg :highlight-cells :initform '())
   (final-state-reached :initarg :final-state-reached :initform nil)
   ))




