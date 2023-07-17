(in-package :loc)

(defvar *positions*   nil) ; will be shown on an offline map (TODO)
(defvar *my-position* nil)

(defun ini ()
  #+android
  (qt:ini-positioning qt:*cpp*)
  #+ios
  (q> |active| ui:*position-source* t)
  #+mobile
  (update-my-position))

#+mobile
(defun last-gps-position ()
  #+android
  (qt:last-position qt:*cpp*)
  #+ios
  (qjs |lastPosition| ui:*position-source*))

#+mobile
(defun update-my-position ()
  "Mobile only: update position from GPS of mobile device."
  (destructuring-bind (lat lon time)
      (last-gps-position)
    (if (zerop lat)
        (qsingle-shot 1000 'update-my-position)
        (let ((pos (list :lat lat
                         :lon lon
                         :time (if (zerop (length time)) 0 (parse-integer time)))))
          (setf *my-position* pos)
          (qlog "position-updated: ~A" pos)
          (set-position (lora:my-num) pos)
          (send-to-radio))))) ; just once on startup (for now)

#+mobile
(defun send-to-radio ()
  (if lora:*config-complete*
      (unless (getf *positions* (lora:my-num))
        (lora:send-position *my-position*))
      (qsingle-shot 1000 'send-to-radio)))

(defun set-position (node pos)
  (let ((lat (getf pos :lat)))
    (when (and node lat (not (zerop lat)))
      (setf (getf *positions* node) pos))))

