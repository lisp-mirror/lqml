(defpackage :qt
  (:use :cl :qml)
  (:export
   #:*ble*
   #:ini
   #:set-device
   #:start-device-discovery
   #:read*
   #:write*))

(in-package :qt)

(defvar *ble* nil)

(defun ini ()
  (setf *ble*
        #+qt-plugin (qload-c++ "cpp/qt")
        #-qt-plugin (qfind-child nil "QT"))
  (let ((*package* (find-package :qt)))
    (define-qt-wrappers *ble*)))

