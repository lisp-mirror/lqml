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

(defvar *newline* #.(coerce (list #\Return #\Newline) 'string))

(defun tile-provider-handler (stream)
  (x:when-it (read-line stream nil nil)
    (when (x:starts-with "GET" x:it)
      (princ (format nil "HTTP/1.0 200 Ok~AContent-Type: application/json; charset=\"utf-8\"~A~A"
                     *newline* *newline* *newline*)
             stream)
      (princ *tile-provider-json* stream)
      (princ *newline* stream)
      (finish-output stream))))

(defvar *tile-provider* nil)

(defun start-tile-provider ()
  (unless *tile-provider*
    (setf *tile-provider*
          (usocket:socket-server (app:my-ip) 1702 'tile-provider-handler nil
                                 :in-new-thread t))))

(defun stop-tile-provider ()
  (when *tile-provider*
    (mp:process-kill *tile-provider*)
    (setf *tile-provider* nil)))

