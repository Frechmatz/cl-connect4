
#|

 #####                                           #       
#     #  ####  #    # #    # ######  ####  ##### #    #  
#       #    # ##   # ##   # #      #    #   #   #    #  
#       #    # # #  # # #  # #####  #        #   #    #  
#       #    # #  # # #  # # #      #        #   ####### 
#     # #    # #   ## #   ## #      #    #   #        #  
 #####   ####  #    # #    # ######  ####    #        #  
                                                         
|#


(defparameter *WIDTH* 7)
(defparameter *HEIGHT* 6)


; Quasiquoting
(defun field-create () (make-array `( ,(+ 2 *HEIGHT*) ,(+ 2 *WIDTH*)) :initial-element 0))

(defun field-init-border  (f)
  (dotimes (x (+ 2 *WIDTH*)) (setf (aref f 0 x) 'X))
  (dotimes (x (+ 2 *WIDTH*)) (setf (aref f (+ 1 *HEIGHT*) x) 'X))
  (dotimes (y (+ 2 *HEIGHT*)) (setf (aref f y 0) 'X))
  (dotimes (y (+ 2 *HEIGHT*)) (setf (aref f y (+ 1 *WIDTH*)) 'X))
)

(print (field-create))

