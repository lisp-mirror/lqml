(defpackage :qt
  (:use :cl :qml)
  (:export
   #:*cpp*
   #:ini
   #:ini-sensors
   #:heart-rate
   #:heart-rate-accuracy))

(in-package :qt)

(defvar *cpp* nil)

(defun ini ()
  #+android
  (progn
    (setf *cpp* (qfind-child nil "QT"))
    (let ((*package* (find-package :qt)))
      (define-qt-wrappers *cpp*))))
