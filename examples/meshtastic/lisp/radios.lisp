(in-package :radios)

(defvar *found* nil)

(defun device-discovered (name)
  "Show discovered (cached) device, which may not be reachable / turned on."
  (unless *found*
    (add-radio
     (list :name name
           :hw-model "Meshtastic" ; we don't know yet
           :current (equal name (app:setting :device))
           :ini t))))

(defun add-radio (radio)
  "Adds passed RADIO (a PLIST) to QML item model.
  The model keys are:
  :name :hw-model :battery-level :current"
  (qjs |addRadio| ui:*radios* radio))

(defun clear ()
  (setf lora:*schedule-clear* nil)
  (q! |clear| ui:*radios*))

(defun change-radio (name) ; see QML
  (app:change-setting :device name)
  (qlater (lambda () (lora:start-device-discovery name)))
  (values))

(defun reset-default-radio ()
  ;; TODO: add in UI settings
  (app:change-setting :device nil))

