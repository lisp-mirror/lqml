(in-package :radios)

(defvar *schedule-clear* nil)

(defun add-radio (radio)
  "Adds passed RADIO (a PLIST) to QML item model.
  The model keys are:
  :name :hw-model :battery-level :current"
  (qjs |addRadio| ui:*radios* radio))

(defun clear ()
  (setf *schedule-clear* nil)
  (q! |clear| ui:*radios*))

(defun change-radio (name) ; called from QML
  (qlater (lambda () (lora:start-device-discovery name)))
  (values))

