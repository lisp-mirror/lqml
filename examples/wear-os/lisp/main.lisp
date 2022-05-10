(in-package :heart-rate)

(defun run ()
  (qt:ini)
  #+android
  (when t (ensure-permissions :body-sensors)
    (qt:ini-sensors qt:*cpp*))
  (update-heart-rate))

;;; runs a demo mode in absence of sensor data (first few seconds on the watch)

(defun update-heart-rate ()
  (q! |stop| ui:*animation*)
  (let* ((bpm #+android (qt:heart-rate qt:*cpp*))
         (iv 1000)
         (demo-mode (or (not bpm) (zerop bpm))))
    (q> |border.color| ui:*main* (if demo-mode "#e04040" "#40c040"))
    (when demo-mode
      (setf bpm 60))
    (let ((ms (truncate (/ 60000 bpm 2))))
      (q> |duration| ui:*zoom-in*  ms)
      (q> |duration| ui:*zoom-out* ms)
      (q> |text| ui:*heart-rate* (princ-to-string bpm))
      (q> |text| ui:*accuracy*   (if demo-mode
                                     "demo"
                                     (princ-to-string (qt:heart-rate-accuracy qt:*cpp*))))
      (setf iv (* 2 ms)))
    (q! |start| ui:*animation*)
    (qsingle-shot iv 'update-heart-rate)))

(qlater 'run)
