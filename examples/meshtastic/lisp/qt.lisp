(defpackage :qt
  (:use :cl :qml)
  (:export
   #:*cpp*
   #:data-path
   #:ini
   #:ini-db
   #:ini-positioning
   #+(or android ios)
   #:keep-screen-on
   #:last-position
   #:local-ip
   #:start-device-discovery
   #:stop-device-discovery
   #:read-ble
   #:set-background-mode ; for testing
   #:set-device-filter
   #:sql-query
   #:write-ble
   #:write-usb))

(in-package :qt)

(defvar *cpp* nil)

(defun ini ()
  (setf *cpp*
        #+qt-plugin (qload-c++ "cpp/qt")
        #-qt-plugin (qfind-child nil "QT"))
  (let ((*package* (find-package :qt)))
    (define-qt-wrappers *cpp*)))
