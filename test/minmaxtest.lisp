


(load "testutil.lisp")
(load "../boardformatter.lisp")
(load "../classic.lisp")


(defun test-direct-win ()
  (format t "*******************************************~%")
  (format t "************* Lets Go *********************~%")
  (format t "*******************************************~%")
  (let ( (board nil) (best-move nil) (*classic-skip-randomizer* t))
    (setf board (create-test-board (list
				    ".."
				    ".w"
				    ".w"
				    "bw")))
    ;;(trace reduce-scores)
    (setf best-move (best-move board *WHITE* 3)) ;; 0,1,2,3
    (format t "~%")
    (format-board board (make-instance 'colorful-cell-formatter))
    (format t "~%Best move is column ~a row ~a with score ~a~%" (first best-move) (second best-move) (third best-move))
    (format t "Expected column of best move is column 1~%")
    ))


(defun test-win-by-two ()
  (format t "*******************************************~%")
  (format t "************* Lets Go *********************~%")
  (format t "*******************************************~%")
  (let ( (board nil) (best-move nil) (*classic-skip-randomizer* t))
    (setf board (create-test-board (list
				    "wb...."
				    "bbww.w"
				    "bbww.w"
				    "bwbb..")))
    ;;(trace reduce-scores)
    (setf best-move (best-move board *WHITE* 3)) ;; 0,1,2,3
    (format t "~%")
    (format-board board (make-instance 'colorful-cell-formatter))
    (format t "~%Best move is column ~a row ~a with score ~a~%" (first best-move) (second best-move) (third best-move))
    ;; 'checkmate' in two draws
    (format t "Expected column of best move is column 4~%")
    ))

(defun test-aggressive ()
  (format t "*******************************************~%")
  (format t "************* Lets Go *********************~%")
  (format t "*******************************************~%")
  (let ( (board nil) (best-move nil) (*classic-skip-randomizer* t))
    (setf board (create-test-board (list
				    ".."
				    ".."
				    ".w"
				    "bw")))
    ;;(trace reduce-scores)
    (setf best-move (best-move board *WHITE* 3)) ;; 0,1,2,3
    (format t "~%")
    (format-board board (make-instance 'colorful-cell-formatter))
    ;; There is no real winning situation here for white
    (format t "~%Best move is column ~a row ~a with score ~a~%" (first best-move) (second best-move) (third best-move))
    (format t "Expected column of best move is column 1~%")
    ))



(test-direct-win)

;; (test-aggressive)

(test-win-by-two)

