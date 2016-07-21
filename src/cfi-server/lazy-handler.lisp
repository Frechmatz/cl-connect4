(in-package :cfi-server)

(defparameter *cycles-to-skip* 1000)

(defun create-lazy-handler
    (interval-seconds
     callback-fn
     &key
       default-return-value
       is-sticky-return-value)
  "Returns a low-cost wrapper function that filters high-frequency calls 
of a given function.
:default-return-value The value to be returned if the call has been filtered.
:is-sticky-return-value If true the default return value is the result of the
latest call of the callback function. If the callback function hasn't be
called yet, the value as defined by :default-return-value is returned."
  (let ((counter 0)
	(start-time (get-internal-real-time))
	(return-value default-return-value))
    (let ((fn (lambda ()
	       (setf counter (+ counter 1))
	       (if (> counter *cycles-to-skip*)
		   (progn
		     (setf counter 0)
		     (let ((cur-time (get-internal-real-time)))
		       (if (>= (/ (- cur-time start-time) internal-time-units-per-second) interval-seconds)
			   (progn
			     (setf start-time cur-time)
			     (let ((result (funcall callback-fn)))
			       (if is-sticky-return-value (setf return-value result))))))))
	       return-value)))
      fn)))
