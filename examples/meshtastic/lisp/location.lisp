(in-package :loc)

(defvar *positions*   nil)
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
  (let* ((pos #+android (qrun* (qt:last-position qt:*cpp*)) ; 'qrun*': return value
              #+ios     (qjs |lastPosition| ui:*position-source*))
         (time (third pos)))
    (when time
      (setf (third pos)
            (if (zerop (length time)) 0 (parse-integer time))))
    pos))

#+mobile
(defun update-my-position (&optional (sec 60)) ; try for 1 min
  "Mobile only: update position from GPS of mobile device."
  (destructuring-bind (lat lon time)
      (last-gps-position)
    (if (zerop lat)
        (unless (zerop sec)
          (qsingle-shot 1000 (lambda () (update-my-position (1- sec)))))
        (let ((pos (list :lat lat :lon lon :time time)))
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

(defun position* (node)
  (when (stringp node)
    (setf node (parse-integer node))) ; for JS
  (x:when-it (getf *positions* node)
    (list (getf x:it :lat)
          (getf x:it :lon))))

(defun set-position (node pos)
  (let ((lat (getf pos :lat)))
    (when (and node lat (not (zerop lat)))
      (setf (getf *positions* node) pos))))

;;; distance

(defconstant +earth-mean-radius+ 6371.0072d0)

(defun to-rad (deg)
  (/ (* deg pi) 180))

(defun distance (from to)
  ;; Haversine formula
  (destructuring-bind ((lat-1 lon-1) (lat-2 lon-2))
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
            (floor (+ 0.5 (* x +earth-mean-radius+ 1000)))))
        0)))

(defun tile-path ()
  (namestring (app:in-data-path "tiles/")))

(defun activate-map ()
  (unless (q< |active| ui:*map-loader*)
    (qt:start-tile-server qt:*cpp*)
    (q> |active| ui:*map-loader* t)
    #+mobile
    (destructuring-bind (lat lon time)
        (last-gps-position)
      (unless (zerop lat)
        (qjs |setCenter| ui:*map* (list lat lon))))
    #-mobile
    (let ((my-pos (position* (lora:my-num))))
      (when my-pos
        (qjs |setCenter| ui:*map* my-pos)))))

(defun show-map-clicked () ; see QML
  (let ((show (not (q< |visible| ui:*map-view*))))
    (when show
      (activate-map)
      (qjs |updatePositions| ui:*map*
           (find-quick-item ui:*group*)))
    (q> |visible| ui:*map-view* show)
    ;; move map (not page) when swiping to left
    (q> |interactive| ui:*main-view* (not show))
    (unless show
      (q> |active| ui:*map-loader* nil)))
  (values))

(defun position-count ()
  (length *positions*))

