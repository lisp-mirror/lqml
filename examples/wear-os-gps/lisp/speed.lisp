(in-package :gps)

(defvar *distance-samples* 5)

(let ((speed 0.0)
      samples)
  (defun update-speed ()
    "After *DISTANCE-SAMPLES* samples, calculate average speed and update on
    every new distance sample."
    (push (cons (distance)
                (get-internal-real-time))
          samples)
    (when (> (length samples) *distance-samples*)
      (setf samples (butlast samples))
      (let ((b (first samples))
            (a (first (last samples))))
        (setf speed (* 3.6                       ; km/h
                       (/ (- (car b) (car a))    ; m
                          (/ (- (cdr b) (cdr a)) ; s
                             1000.0)))))))
  (defun speed* ()
    speed))
