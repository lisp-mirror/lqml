(in-package :loc)

(defvar *positions*   nil)
(defvar *my-position* nil)

(defun ini ()
  #+android
  (qt:ini-positioning qt:*cpp*)
  #+(or ios sfos)
  (q> |active| ui:*position-source* t)
  #+(or android ios sfos)
  (update-my-position))

#+(or android ios sfos)
(defun last-gps-position ()
  (let* ((pos #+android (qrun* (qt:last-position qt:*cpp*)) ; 'qrun*': return value
              #+ios     (qjs |lastPosition| ui:*position-source*))
         (time (third pos)))
    (when (stringp time)
      (setf (third pos) (parse-integer time))) ; see QML
    pos))

#+(or android ios sfos)
(defun update-my-position (&optional (sec 60)) ; try for 1 min
  "Mobile only: update position from GPS of mobile device."
  ;; see also Timer in 'qml/ext/Radios.qml'
  (destructuring-bind (lat lon time)
      (last-gps-position)
    (if (zerop lat)
        (unless (zerop sec)
          (qsingle-shot 1000 (lambda () (update-my-position (1- sec)))))
        (let ((pos (list :lat lat :lon lon :time time)))
          (setf *my-position* pos)
          (qlog "position-updated: ~A" pos)
          (set-position (lora:my-num) pos)
          (send-to-radio)))))

#+(or android ios sfos)
(defun send-to-radio ()
  (if lora:*config-complete*
      (unless (getf *positions* (lora:my-num))
        (lora:send-position *my-position*))
      (qsingle-shot 1000 'send-to-radio)))

(defun position* (node)
  (when node
    (x:when-it (getf *positions* node)
      (list (getf x:it :lat)
            (getf x:it :lon)))))

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

(defun tile-path () ; see QML
  (namestring (app:in-data-path "" "tiles/")))

(defun tile-provider-path () ; see QML
  (if (probe-file "qml/tile-provider/")
      (x:cc "file://" (namestring (merge-pathnames "qml/tile-provider/"))) ; development
      "qrc:///qml/tile-provider/"))                                        ; final app

(defun activate-map ()
  (unless (q< |active| ui:*map-loader*)
    (q> |active| ui:*map-loader* t)
    #+(or android ios sfos)
    (destructuring-bind (lat lon time)
        (last-gps-position)
      (unless (zerop lat)
        (qjs |setCenter| ui:*map* (list lat lon))))
    #-(or android ios sfos)
    (let ((my-pos (position* (lora:my-num))))
      (when my-pos
        (qjs |setCenter| ui:*map* my-pos)))))

(defun show-map-clicked () ; see QML
  (let ((show (not (q< |visible| ui:*map-view*))))
    (when show
      (activate-map)
      (x:when-it (lora:my-num)
        (qjs |updatePositions| ui:*map*
             x:it
             (lora:my-name)
             (find-quick-item ui:*group*))))
    (q> |visible| ui:*map-view* show)
    ;; move map (not page) when swiping to left
    (q> |interactive| ui:*main-view* (not show))
    (unless show
      (q> |active| ui:*map-loader* nil)))
  (values))

(defun position-count () ; see QML
  (length *positions*))

;;; save/restore tiles

(defun copy-stream (from to &optional (size most-positive-fixnum))
  (let* ((buf-size (min 8192 size))
         (buf (make-array buf-size :element-type (stream-element-type from))))
    (loop :for pos = (read-sequence buf from :end (min buf-size size))
          :do (write-sequence buf to :end pos)
              (decf size pos)
          :until (or (zerop pos)
                     (zerop size))))
  (values))

(defun make-map-bin ()
  "Writes all tiles in a single file, because images are already compressed.
  This is meant to avoid useless (and possibly slow) zipping."
  (with-open-file (out (app:in-data-path app:*backup-map-file* "backup/")
                       :direction :output :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (let ((directories (directory (app:in-data-path "**/" "tiles/"))))
      (when directories
        (let ((p (search "tiles/" (namestring (first directories)) :from-end t)))
          (flet ((add-string (str)
                   (write-sequence (x:string-to-bytes str) out))
                 (sep ()
                   (write-byte #.(char-code #\|) out)))
            (dolist (dir directories)
              (add-string (subseq (namestring dir) p))
              (sep))
            (let ((files (directory (app:in-data-path "**/*.*" "tiles/"))))
              (dolist (file files)
                (with-open-file (in file :element-type (stream-element-type out))
                  (add-string (subseq (namestring file) p))
                  (sep)
                  (add-string (princ-to-string (file-length in)))
                  (sep)
                  (copy-stream in out))))))))))

(defun extract-map-bin (&optional delete)
  "Restores tiles from a previously saved single binary file named 'map.bin'."
  (let ((blob (app:in-data-path app:*backup-map-file* "")))
    (when (probe-file blob)
      (with-open-file (in blob :element-type '(unsigned-byte 8))
        (flet ((read-string ()
                 (let ((bytes (loop :for byte = (read-byte in nil nil)
                                    :while (and byte (/= byte #.(char-code #\|)))
                                    :collect byte)))
                   (if bytes
                       (x:bytes-to-string bytes)
                       (progn
                         (when delete
                           (close in)
                           (delete-file blob))
                         (return-from extract-map-bin))))))
          (loop
            (let ((name (app:in-data-path (read-string) "")))
              (if (cl-fad:directory-pathname-p name)
                  (ensure-directories-exist name)
                  (let ((size (parse-integer (read-string))))
                    (with-open-file (out name :direction :output :if-exists :supersede
                                              :element-type (stream-element-type in))
                      (copy-stream in out size)))))))))))

