;;; this requires a CLOG fork prepared for mobile

(in-package :clog)

(setf clog-connection::*send-to-webview*
      #+ios (lambda (js) (qml:qjs |send| ui:*server* js))
      #-ios (lambda (js) (qml:q! |runJavaScript| ui:*browser* js)))

(defun webview/on-new-connection ()
  (clog-connection::handle-new-connection 'qml-webview nil))

(defun webview/on-message (message)
  (clog-connection::handle-message 'qml-webview message))

(defun webview/on-close ()
  (clog-connection::handle-close-connection 'qml-webview))

(defun boot ()
  (qml:q> |url| ui:*browser* (format nil "file://~A"
                                     (merge-pathnames "htm/boot.html"))))

(export 'boot)
