

(in-package :connect4-ccfi-console)

(defun best-move (placement players-color)
  (let ((board (ccfi:ccfi-placement-to-board placement)))
    (let ((result (connect4-api:play board (ccfi:ccfi-token-to-color players-color) 6)))
      (if result
	  (first result)
	  nil))))

;;; TODO: Error handling
(defun process-queue (command-queue)
  (let ((cur-placement nil) (cur-players-color nil))
    (dolist (command command-queue)
      (let ((items (cl-ppcre:split " " command)))
	(if items
	    (if (equal (first items) "position")
		(progn
		  (setf cur-placement (second items))
		  (setf cur-players-color (third items)))
		(if (equal (first items) "newgame")
		    (progn
		      (setf cur-placement nil)
		      (setf cur-players-color nil)))))))
    (if cur-placement (format nil "bestmove ~a" (best-move cur-placement cur-players-color)) nil)
    ))


(defun game-loop ()
  (let ((str nil) (command-queue '()))
    (labels ((do-cmd ()
	   (setf str (read-line))
	     (if (equal str "quit")
		 nil
		 (progn
		   (if (equal str "go")
		       (progn
			 (format t "~a~%" (process-queue (nreverse command-queue)))
			 (setf command-queue '()))
		       (push str command-queue))
		   (do-cmd)))))
      (do-cmd)
    )))

(defun start ()
  (format t "~%Server has been started. Enter quit to quit~%")
  (finish-output)
  (game-loop)
  (format t "Bye~%")
  )

  






