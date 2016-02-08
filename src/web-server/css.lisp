(in-package :connect4-css)


(defun body-background-color ()
  "linen"
  )

(defun mausi-style ()
  (cl-css:css '(
		(.mausi :background-color "mausi"))))


(defun css ()
  (cl-css:css `(
		(.header
		 :background-color "LightGoldenRodYellow"
		 :background-image "url(/static/made-with-lisp-logo.png)"
		 :background-repeat "no-repeat"
		 :background-size "contain"
		 :height "100px"
		 :background-position "right"
		 ;;:font-size "5vw"
		 :position relative
		 )
		(.board :background-color "black")
		(body :background-color ,(body-background-color))
		(".header h1" 
			 :margin "0"
			 :position "absolute"
			 :top "50%"
			 :-webkit-transform "translate(0, -50%)" ;;; Safari
			 :transform "translate(0, -50%)"
			 )
		(.board
		 :width "40%"
		 :float "left"
		 )
		(.board-table
		 :width "100%"
		 )
		(.board-cell-content
		 :height 80%
		 :width 80%
		 :padding 10% 10%
		 )
		(,(format nil ".board-cell-content[token=~ax~a]" #\" #\" )
		 :background-color "red")
		(,(format nil ".board-cell-content[token=~a_~a]" #\" #\" )
		 :background-color "white")
		(,(format nil ".board-cell-content[token=~ao~a]" #\" #\" )
		 :background-color "green")
		(.console
		 :width "30%"
		 :float "left"
		 )
		(,(mausi-style))
	      )))
