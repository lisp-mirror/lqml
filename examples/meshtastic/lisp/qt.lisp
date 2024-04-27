(defpackage :qt
  (:use :cl :qml)
  (:export
   #:*cpp*
   #:data-path
   #:disconnect
   #:ini
   #:ini-db
   #:ini-positioning
   #+(or android ios)
   #:keep-screen-on
   #:last-position
   #:local-ip
   #:start-device-discovery
   #:stop-device-discovery
   #:read*
   #:set-connection-type
   #:set-device-filter
   #:sql-query
   #:write*))

(in-package :qt)

(defvar *cpp* nil)

(defun ini ()
  (setf *cpp*
        #+qt-plugin (qload-c++ "cpp/qt")
        #-qt-plugin (qfind-child nil "QT"))
  (let ((*package* (find-package :qt)))
    (define-qt-wrappers *cpp*)))
