
(in-package :connect4-buttons)

(defun get-debug-button ()
  ;; Width, Height = 60 + Border
  (let ((scene (cl-svg:make-svg-toplevel 'svg-1.1-toplevel :height 70 :width 70)))
    (make-group scene (:transform "translate(5,5)")
      (draw* (:circle :cx 29 :cy 29 :r 30 :stroke "red" :fill "transparent" :stroke-width "10"))
      (draw* (:circle :cx 29 :cy 29 :r 15 :stroke "red" :fill "transparent" :stroke-width "5"))
      )
    (let ((str (with-output-to-string (s nil) (cl-svg:stream-out s scene))))
      str)))

(defun get-start-new-game-button ()
  ;; Width, Height = 60 + Border
  (let ((scene (cl-svg:make-svg-toplevel 'svg-1.1-toplevel :height 70 :width 70)))
    (make-group scene (:transform "translate(5,5)")
      ;; Draws a "X" into a 60*60 grid, starting from 0,0 
      (draw* (
	      :polygon
	      :stroke "red"
	      :fill "transparent"
	      :stroke-width "10"
	      :points "9 0 29 19 49 0 59 9 39 29 59 49 49 59 29 39 9 59 0 49 19 29 0 9")))
    (let ((str (with-output-to-string (s nil) (cl-svg:stream-out s scene))))
      str)))
