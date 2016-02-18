(in-package :connect4-css)


(defun body-background-color ()
  "linen"
  )

(defun display-flex (selector)
  (cl-css:css `((,selector
    :display "flex"
    :display "-webkit-flex"
))))

(defun flex (selector)
  (cl-css:css `((,selector
    :flex "1"
    :-webkit-flex "1"
))))

(defun css ()
  (cl-css:css `(
		
		(.page-wrapper
		 :padding "20px"
		 )
		
		;; Placement of main blocks
		(.header :height "20vh")
		(.footer :height "10vh"
			 :margin-top "20px"
			 )
		(.body
		 :margin-top "20px"
		 ;; 100%
		 ;; - 2*WrapperPadding
		 ;; - HeaderHeight
		 ;; - FooterHeight
		 ;; - FooterMargin
		 ;; - BodyMargin
		 :height "calc(100vh - 40px - 20vh - 10vh - 20px - 20px)"
		 )

		
		;;
		;; Body: Consists of Navbar, Playground and CCFI Console
		;;

		;; Playground (board)
		(.body
		 :flex-flow "row"
		 :width "100%"
		 )
		(,(display-flex ".body"))

		(".body .navbar"
		 :width "100px")
		(".body .board"
		 :width "20vh" ;; Breite des Feldes = Höhe des Parents
		 :background-color "green"
		 ;;:flex "1"
		 )
		(,(flex ".body .board"))
		
		;; CCFI console
		(".body .console"
		 :width "20em"
		 :background-color "yellow"
		 ;;:flex 1
		 ;;:-webkit-flex 1
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

		;; Styling navbar
		(.navbar
		 :background-color "red"
		 )
		
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
