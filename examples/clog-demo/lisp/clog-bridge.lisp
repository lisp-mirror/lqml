;;; this requires a CLOG fork prepared for mobile

(in-package :clog)

(setf clog-connection::*send-to-webview*
      #+ios (lambda (js) (qml:qjs |send| ui:*server* js))
      #-ios (lambda (js) (qml:q! |runJavaScript| ui:*browser* js nil)))

(defun webview/on-new-connection ()
  (clog-connection::handle-new-connection 'qml-webview nil))

(defun webview/on-message (message)
  (clog-connection::handle-message 'qml-webview message))

(defun boot ()
  (qml:q> |url| ui:*browser*
          #+android "file:///android_asset/lib/static-files/boot.html"
          #-android (format nil "file://~A" (merge-pathnames "static-files/boot.html"))))

(export 'boot)
