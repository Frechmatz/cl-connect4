
(in-package :connect4)

(defclass message-formatter () ())
(defgeneric format-message (message-formatter message)
  (:documentation "Formats a text message"))
(defmethod format-message ( (formatter message-formatter) message)
  (format t "~a~%" message))
(defclass colorful-message-formatter (message-formatter) ()
  (:documentation "Formats a text message using ANSI escape sequences for colored output"))
(defmethod format-message ( (formatter colorful-message-formatter) message)
  (format t "~c[32m~a~c[0m~%" #\Esc message #\Esc))

