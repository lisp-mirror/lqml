(in-package :loc)

(defvar *tile-provider-json*
"{
 \"UrlTemplate\": \"https://tile.openstreetmap.org/%z/%x/%y.png\",
 \"ImageFormat\": \"png\",
 \"QImageFormat\": \"Indexed8\",
 \"ID\": \"cl-meshtastic\",
 \"MaximumZoomLevel\": 19,
 \"MapCopyRight\": \"<a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>\",
 \"DataCopyRight\": \"\"
 }")

(defvar *newline* #.(format nil "~C~C" #\Return #\Newline))

(defun tile-provider-handler (stream)
  (when (x:starts-with "GET" (read-line stream))
    (princ (format nil "HTTP/1.0 200 Ok~AContent-Type: application/json; charset=\"utf-8\"~A~A"
                   *newline* *newline* *newline*)
           stream)
    (princ *tile-provider-json* stream)
    (princ *newline* stream)
    (finish-output)))

(defvar *tile-provider* nil)

(defun start-tile-provider ()
  (unless *tile-provider*
    (setf *tile-provider*
          (mp:process-run-function
           :tile-provider
           (lambda () (usocket:socket-server (app:my-ip) 1702 'tile-provider-handler))))))

