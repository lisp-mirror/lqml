(in-package :radios)

(defun add-radio (radio)
  "Adds passed RADIO (a PLIST) to QML item model.
  The model keys are:
  :name :hw-model :battery-level :current"
  (qjs |addRadio| ui:*radios* radio))

(defun clear ()
  (setf lora:*schedule-clear* nil)
  (q! |clear| ui:*radios*))

(defun change-radio (name) ; called from QML
  (qlater (lambda () (lora:start-device-discovery name)))
  (values))

(defun reset-configured ()
  ;; TODO: add in UI settings
  (app:change-setting :configured nil))

