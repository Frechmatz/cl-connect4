(in-package :connect4-css)


(defun body-background-color ()
  "linen"
  )

(defun display-flex (selector)
  (cl-css:css `((,selector
    :display "flex"
    :display "-webkit-flex"
))))

(defun css ()
  (cl-css:css `(
		
		(.page-wrapper :padding "20px")
		
		;; Placement of main blocks
		(.header :height "20vh")
		(.body :margin-top "20px")
		(.footer :margin-top "20px")

		
		;;
		;; Body: Consists of Playground and CCFI Console
		;;
		
		;; box layout of body
		;; ;; Playground (board)
		;; (".body .board" :width "40%" :float "left")
		;; ;; CCFI console
		;; (".body .console" :width "30%" :float "right"
		;; 		  :height "200px"
		;; 		  )

		;; flex layout of body
		;; Playground (board)
		(.body
		 :flex-flow "row"
		 :height "60vh" ;; Höhe des Bodies = 60% 
		 )
		(,(display-flex ".body"))

		
		(".body .board"
		 :width "100%" ;; Breite des Feldes = Höhe des Parents
		 ;; :flex "1"
		 )
		;; CCFI console
		(".body .console"
		 ;;:width "100px"
		 :background-color "yellow"
		 :flex 1
		 :-webkit-flex 1
		 ;;:display "flex"
		 )

		(".footer" :clear "both" :color "yellow") 
		
		;;
		;; Inner playground
		;;
		(".board .board-table" :width "100%")

		;;
		;; Inner CCFI console
		;;
		(".console .console-content"
		 :width "100%"
		 :height "100%"
		 ;;:display "flex"
		 )
		(".console .console-textarea"
		 :width "100%"
		 :height "100%"
		 ;;:flex "1"
		 )
		
		;; Styling: Body
		(body :background-color ,(body-background-color))

		;; Styling: Header
		(.header
		 :background-color "blue"
		 ;; :background-image "url(/static/made-with-lisp-logo.png)"
		 :background-image "url(/static/made-with-lisp-ellipse.png)"
		 :background-repeat "no-repeat"
		 :background-size "contain"
		 :background-position "right"
		 :position relative
		 )
		(".header h1" 
		 :margin "0"
		 :color "yellow"
		 :position "absolute"
		 :top "50%"
		 :-webkit-transform "translate(0, -50%)" ;;; Safari
		 :transform "translate(0, -50%)"
		 )

		;; Styling: Footer
		(.footer
		 :background-color "blue")

		;; Styling: Console
		(.console-textarea
		 :background-color "black"
		 :color "lime"
		 )

		;; Styling: Board
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
	      )))
