

(defpackage :cfi-server
  (:use :cl :cl-ppcre :alexandria :bordeaux-threads :board :engine)
  (:export :create-placement)
  (:export :cfi-server)
  (:export :put)
  (:export :message)
  (:export :start)
  (:export :stop)
  (:export :get-state)
  (:export :+SERVER-STATE-STOPPED+)
  )

