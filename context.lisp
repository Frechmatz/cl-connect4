
(in-package :connect4)

;;;;
;;;; The Game Context
;;;;

(defconstant GAME-STATE-FINAL 1)
(defconstant GAME-STATE-CONTINUE 2)
(defconstant GAME-STATE-PROCESSING-FINAL 3)

(defclass context ()
  (
   (board :accessor board)
   (players-color :accessor players-color)
   (difficulty-level :accessor difficulty-level)
   (state :initarg :state :initform GAME-STATE-CONTINUE :accessor state)
   (wins :initform 0 :accessor wins)
   (loses :initform 0 :accessor loses)
   (draws :initform 0 :accessor draws)
   ))

