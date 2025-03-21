(defpackage :qt
  (:use :cl :qml)
  (:export
   #:*cpp*
   #:data-path
   #:ini
   #:ini-db
   #:sql-query))

(in-package :qt)

(defvar *cpp* nil)

(defun ini ()
  (setf *cpp*
        #+qt-plugin (qload-c++ "cpp/qt")
        #-qt-plugin (qfind-child nil "QT"))
  (let ((*package* (find-package :qt)))
    (define-qt-wrappers *cpp*)))
