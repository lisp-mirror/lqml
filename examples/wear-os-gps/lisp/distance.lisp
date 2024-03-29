(in-package :gps)

(defconstant +earth-mean-radius+ 6371.0072d0)

(defun to-rad (deg)
  (/ (* deg pi) 180))

(defun to-deg (rad)
  (/ (* 180 rad) pi))

(defun distance (from to)
  "Coordiante distance according to Haversine formula."
  (destructuring-bind ((lat-1 . lon-1) (lat-2 . lon-2))
      (list from to)
    (if (and (numberp lat-1)
             (numberp lat-2))
        (let* ((dlat (to-rad (- lat-2 lat-1)))
               (dlon (to-rad (- lon-2 lon-1)))
               (h-dlat (sin (/ dlat 2)))
               (h-dlon (sin (/ dlon 2))))
          (setf h-dlat (expt h-dlat 2)
                h-dlon (expt h-dlon 2))
          (let* ((y (+ h-dlat (* (cos (to-rad lat-1))
                                 (cos (to-rad lat-2))
                                 h-dlon)))
                 (x (* 2 (asin (sqrt y)))))
            (* x +earth-mean-radius+ 1000)))
        0)))

(let ((positions-distance 0)
      (distance 0)
      position-1
      position-2
      constant-direction)
  (defun update-distance ()
    "Calculate distance of farest points with same direction
    (fallback: re-calculate after a certain distance)."
    (when (and *direction*
               (not constant-direction))
      (setf constant-direction *direction*))
    (let ((pos (cons kal:*lat* kal:*lon*))
          (reset (if constant-direction
                     (or (>= (abs (- *direction* constant-direction))
                             15)                     ; not too small (not accurate with low speed)
                         (>= positions-distance 75)) ; 75 m
                     (>= positions-distance 25))))   ; 25 m
      (if reset
          (progn
            (setf constant-direction *direction*)
            (if position-1
                (progn
                  (setf position-2 pos)
                  (incf distance (positions-distance))
                  (setf positions-distance 0)
                  (shiftf position-1 position-2 nil))
                (setf position-1 pos)))
          (progn
            (if position-1
                (setf position-2 pos)
                (setf position-1 pos))
            (setf positions-distance (positions-distance))))))
  (defun positions-distance ()
    (if position-2
        (point-distance position-1 position-2)
        0))
  (defun distance ()
    (+ distance positions-distance)))
