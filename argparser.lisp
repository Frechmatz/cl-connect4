

;;
;; Returns list of parsed arguments
;; Arguments are transformed by attached parsers
;;

(define-condition invalid-arguments (error)
  ((text :initarg :text :reader text)))

(defun parse-arguments (args parsers context)
  (let ((result ()))
    (labels ((parse (args parsers context)
		  (let ((arg (car args)) (parser (car parsers)))
		    (cond 
		     ((and (not arg) (not parser)) nil)
		     ((and (not arg) parser) (error 'invalid-arguments :text "Missing arguments"))
		     ((and arg (not parser)) (error 'invalid-arguments :text "Too many arguments"))
		     (t 
			(push (funcall parser arg context) result)
			(parse (cdr args) (cdr parsers) context))
		     )
		    )
		  )
	   )
	  ;; body of labels
	  (parse args parsers context)
	  )
    ;; body of let
    (reverse result)
    ))


