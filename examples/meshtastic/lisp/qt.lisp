(defpackage :qt
  (:use :cl :qml)
  (:export
   #:*cpp*
   #:ini
   #:ini-db
   #:ini-positioning
   #:last-position
   #:local-ip
   #:start-device-discovery
   #:read*
   #:short-names
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
