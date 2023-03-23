(in-package :qml-user)

;;; low-pass filter

(defun make-low-pass-filter (&optional (alpha 0.02))
  (let ((a alpha)
        filtered)
    (lambda (value)
      (setf filtered (if filtered
                         (+ (* value a)
                            (* filtered (- 1.0 a)))
                         value)))))

(defvar *accel-x* (make-low-pass-filter))
(defvar *accel-y* (make-low-pass-filter))
(defvar *accel-z* (make-low-pass-filter))

;;; sensor data

(defun hr (x)
  "Human readable."
  (floor (+ 0.5 (* 1000 x))))

(defun accel-changed (x y z)
  (qlog :x (hr (funcall *accel-x* x)))
  (qlog :y (hr (funcall *accel-y* y)))
  (qlog :z (hr (funcall *accel-z* z))))

