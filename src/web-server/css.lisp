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

#|
(defun cssklein ()
  (ollicss '((.page-wrapper :padding "20px")
	     (.body :flex-flow "row" :display "flex" :flex 1))))
|#

(defun css-safarify (expr)
  (mapcar #'(lambda (statement)
	      (let ((to-append nil))
		(mapcar #'(lambda (item)
			    (cond ((and (keywordp item)
					(equal :flex item))
				   (push :-webkit-flex to-append)
				   (push (getf (cdr statement) :flex) to-append))
				  ((and (stringp item)
					(equalp "flex" item))
				   (push :display to-append)
				   (push "-webkit-flex" to-append))
				  (T item)))
			statement)
		(append statement (reverse to-append))))
	  expr))

(defun css ()
  (cl-css:css
   (css-safarify
    `((.page-wrapper :padding "20px")
      ;; Placement of main blocks
      (.header :height "20vh" :width "100%")
      (.footer :height "10vh" :margin-top "20px" :width "100%")
      (.body
       :width "100%"
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
      ;; Layout of Body: Navbar, Playground, Console
      ;;
      (.body :flex-flow "row" :display "flex")
      (".body .navbar" :width "60px" :margin-right "20px")
      (".body .console" :width "20em" :margin-left "20px")
      (".body .playground" :flex 1)
      
      ;;
      ;; Inner playground
      ;;
      (".board .board-table" :width "40%")

      ;;
      ;; Inner CCFI console
      ;;
      (".console .console-content"
       :width "100%"
       :height "100%"
       )
      (".console .console-textarea"
       :width "100%"
       :height "100%"
       )

      ;; Styling Background
      (.page-wrapper :background-color "yellow")
      
      ;; Styling: Body
      (body :background-color ,(body-background-color))

      ;; Styling navbar
      (.navbar
       :background-color "blue"
       )
      (".navbar span"
       :color "yellow"
       )
      (".navbar a" 
       :background-repeat "no-repeat"
       :display "block"
       :width "50px"
       :height "50px"
       :background-size "contain"
       :margin "5px"
       )
      (".navbar a span"
       :display "none"
       )
      (".navbar .link-new-game" 
       :stroke-width "20"
       :background-image "url('/buttons/newgame.svg')"
       )
      (".navbar .link-debug" 
       :background-image "url('/buttons/debug.svg')"
       )
      
      ;; Styling playground 
      (".body .playground" :background-color "violet" )
      
      ;; Styling: Header
      (.header
       :background-color "blue"
       :background-image "url(/static/made-with-lisp-ellipse.png)"
       :background-repeat "no-repeat"
       :background-size "contain"
       :background-position "right"
       :position relative
       )
      (".header h1" 
       :margin "0"
       :margin-left "5px"
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
      (".body .console" :background-color "yellow")

      ;; Styling: Board
      (.board-table
       :border "1px solid black"
       :border-collapse "collapse"
       )
      (".board-table th, .board-table td"
       :border "1px solid black"
       :border-collapse "collapse"
       )
      (,(format nil ".board-cell[data-token=~ax~a]" #\" #\" )
	:background-color "red")
      (,(format nil ".board-cell[data-token=~a_~a]" #\" #\" )
	:background-color "white")
      (,(format nil ".board-cell[data-token=~ao~a]" #\" #\" )
	:background-color "green")
      ))))

