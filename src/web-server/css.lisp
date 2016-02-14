(in-package :connect4-css)


(defun body-background-color ()
  "linen"
  )

(defun mausi-style ()
  (cl-css:css '(
		(.mausi :background-color "mausi"))))


(defun css ()
  (cl-css:css `(
		
		(.page-wrapper :padding "20px")
		
		;; Placement of main blocks
		(.header :height "20%")
		(.body :margin-top "20px")
		(.footer :margin-top "20px")

		
		;;
		;; Body: Consists of Playground and CCFI Console
		;;
		
		;; Playground (board)
		(".body .board" :width "40%" :float "left")
		;; CCFI console
		(".body .console" :width "30%" :float "right"
				  :height "200px"
				  )
		(".footer" :clear "both" :color "yellow") 
		
		;;
		;; Inner playground
		;;
		(".board .board-table" :width "100%")

		;;
		;; Inner CCFI console
		;;
		(".console .console-content" :width "100%":height "100%")
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
		 :background-color "blue")
		(body :background-color ,(body-background-color))
		(".header h1" 
		 :margin "0"
		 :color "yellow"
		 :position "absolute"
		 :top "50%"
		 :-webkit-transform "translate(0, -50%)" ;;; Safari
		 :transform "translate(0, -50%)"
		 )
		(.console-textarea
		 :background-color "black"
		 :color "lime"
		 )

		;; Board styling
		;; https://www.w3.org/wiki/Styling_tables
		(.board-table
		 :border "1px solid black;"
		 :border-collapse "collapse"
		 )
		(".board-table th, .board-table td"
		 :border "1px solid black;"
		 :border-collapse "collapse"
		 )
		(,(format nil ".board-cell[token=~ax~a]" #\" #\" )
		 :background-color "red")
		(,(format nil ".board-cell[token=~a_~a]" #\" #\" )
		 :background-color "white")
		(,(format nil ".board-cell[token=~ao~a]" #\" #\" )
		 :background-color "green")
		(,(mausi-style))
	      )))
