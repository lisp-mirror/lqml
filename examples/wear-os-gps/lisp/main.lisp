(in-package :gps)

(defvar *start-time* nil)
(defvar *direction*  nil)
(defvar *log-stream* nil)

(defun run ()
  (qt:ini)
  (load-settings)
  #+mobile
  (progn
    (qlater (lambda () (qt:keep-screen-on qt:*cpp*))) ; delay until UI is loaded (see 'Settings.qml')
    #+android
    (ensure-permissions :access-fine-location))
  (q> |ready| ui:*position-source* t))

(defun closing ()
  (close *log-stream*)
  (qquit))

(defun round* (x &optional (digits 0))
  (when (numberp x)
    (let* ((f (expt 10 digits))
           (r (/ (truncate (+ 0.5 (* f x)))
                 f)))
      (if (zerop digits)
          r
          (float r)))))

(defun str (x)
  (cond ((stringp x)
         x)
        ((null x)
         "")
        (t
         (princ-to-string x))))

(defun position-changed (lat lon accuracy speed direction timestamp)
  (unless *start-time*
    (setf *start-time* timestamp)
    (setf *log-stream* (open (format nil "LOG ~A.csv" (substitute #\. #\: timestamp))
                             :direction :output :if-does-not-exist :create))
    (format *log-stream* "~A,~A,~A,~A,~A,~A,~A,~A,~A,~A~%"
            "timestamp"
            "gps latitude" "gps longitude" "gps h-accuracy (m)"
            "gps speed (m/s)"
            "gps direction (°)"
            "kalman latitude" "kalman longitude"
            "speed (km/h)"
            "distance (m)"))
  (when (and lat lon accuracy)
    (when direction
      (setf *direction* direction))
    (kal:filter lat lon accuracy speed)
    (update-distance)
    (update-speed)
    (q> |value| gauge:*gauge* (speed*))
    (q> |text| ui:*distance* (str (round* (distance))))
    (q> |text| ui:*accuracy* (str (round* accuracy 1)))
    (when kal:*lat* 
      (format *log-stream* "~A,~F,~F,~F,~A,~A,~F,~F,~F,~D~%"
              timestamp
              lat lon (round* accuracy 1)
              (str (round* speed 1))
              (str (round* direction))
              kal:*lat* kal:*lon*
              (round* (speed*) 1)
              (round* (distance))))))

(defun always-on-changed (on) ; called from QML
  #+mobile
  (qt:keep-screen-on qt:*cpp* on))

(defun set-max-speed (index) ; called from QML
  (if (zerop index)
      (q> |maximumValue| gauge:*gauge*
          (q< |value| ui:*max-speed*))
      (q> |value| ui:*max-speed*
          (q< |maximumValue| gauge:*gauge*))))

;;; settings

(let ((file "settings.exp"))
  (defun save-settings () ; called from QML
    (with-open-file (s file :direction :output :if-exists :supersede)
      (print (list (cons :max-speed
                         (q< |maximumValue| gauge:*gauge*))
                   (cons :always-on
                         (q< |checked| ui:*always-on*)))
             s))
    (values)) ; no return value to QML
  (defun load-settings ()
    (when (probe-file file)
      (with-open-file (s file)
        (let ((settings (read s)))
          (q> |maximumValue| gauge:*gauge*
              (cdr (assoc :max-speed settings)))
          (q> |checked| ui:*always-on*
              (cdr (assoc :always-on settings))))))))

(qlater 'run)
