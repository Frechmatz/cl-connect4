(in-package :connect4-css)


(defun body-background-color ()
  "linen"
  )

(defun mausi-style ()
  (cl-css:css '(
		(.mausi :background-color "mausi"))))


(defun css ()
  (cl-css:css `(
		;; Placement of main blocks
		(.header :height "20%")
		(.body :height "70%;")
		(.footer :height "10%")

		
		;;
		;; Body: Consists of Playground and CCFI Console
		;;
		
		;; Playground (board)
		(".body .board" :width "40%" :float "left" :margin-top "40px")
		;; CCFI console
		(".body .console" :width "30%" :float "right" :height "100%")

		;;
		;; Inner playground
		;;
		(".board .board-table" :width "100%")
		(".board-table .board-cell-content" :height 80% :width 80% :padding 10% 10%)

		;;
		;; Inner CCFI console
		;;
		(".console .console-content" :width "100%" :height "100%")
		(".console .console-textarea" :width "100%" :height "100%")
		
		;;
		;; Colors, fonts, background images, etc
		;;
		(.header
		 :background-color "blue"
		 ;; :background-image "url(/static/made-with-lisp-logo.png)"
		 :background-image "url(/static/made-with-lisp-ellipse.png)"
		 :background-repeat "no-repeat"
		 :background-size "contain"
		 :background-position "right"
		 :position relative
		 )
		(.footer
		 :background-color "red")
		(body :background-color ,(body-background-color))
		(".header h1" 
			 :margin "0"
			 :position "absolute"
			 :top "50%"
			 :-webkit-transform "translate(0, -50%)" ;;; Safari
			 :transform "translate(0, -50%)"
			 )
		(.console-textarea
		 :background-color "black"
		 :color "green"
		 )
		(.board-cell-content
		 :border "solid")
		(,(format nil ".board-cell-content[token=~ax~a]" #\" #\" )
		 :background-color "red")
		(,(format nil ".board-cell-content[token=~a_~a]" #\" #\" )
		 :background-color "white")
		(,(format nil ".board-cell-content[token=~ao~a]" #\" #\" )
		 :background-color "green")
		(,(mausi-style))
	      )))
