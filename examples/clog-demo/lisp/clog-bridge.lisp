;;; this requires a CLOG fork prepared for mobile

(in-package :clog-connection)

(setf *websocket-server-send*
      (lambda (text) (qml:qjs |send| ui:*server* text)))

(defun server/on-new-connection ()
  (handle-new-connection 'qml-websocket-server nil))

(defun server/on-message (message)
  (handle-message 'qml-websocket-server message))

(defun server/on-close ()
  (handle-close-connection 'qml-websocket-server))

(in-package :clog)

(defun boot ()
  (qml:q> |url| ui:*browser* (format nil "file://~A"
                                     (merge-pathnames "htm/boot.html"))))

(export 'boot)
