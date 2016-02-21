
(defpackage :connect4-board-renderer
  (:use :cl)
  (:export :RENDER-CCFI-BOARD)
  )

(defpackage :connect4-css
  (:use :cl)
  (:export :CSS)
  )

(defpackage :connect4-javascript
  (:use :cl)
  (:export :javascript)
  )

(defpackage :connect4-ccfi-websocket
  (:use :cl :hunchensocket)
  )

(defpackage :connect4-buttons
  (:use :cl :cl-svg)
  (:export :get-debug-button)
  (:export :get-start-new-game-button)
  )

(defpackage :connect4-web-server
  (:use :cl)
  (:export :start)
  (:export :stop)
  )
