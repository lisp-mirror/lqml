(in-package :radios)

(defvar *found* nil)

(defun ini ()
  (q> |model| ui:*region*
      (cons "-" (mapcar 'symbol-name (rest (lora:keywords :region-code)))))
  (x:when-it (app:setting :region)
    (q> |currentIndex| ui:*region*
        (q! |indexOfValue| ui:*region*
            (symbol-name x:it))))
  (x:when-it (app:setting :device-filter)
    (qt:set-device-filter qt:*cpp* x:it)))

(defun choose-region ()
  (q> |currentIndex| ui:*main-view* 2) ; 'Radios'
  (q! |popup.open| ui:*region*))

(defun device-discovered (name) ; see Qt
  "Show discovered (cached) device, which may not be reachable / turned on."
  (unless *found*
    (add-radio
     (list :name name
           :hw-model "Meshtastic" ; we don't know yet
           :current (equal name (app:setting :device))
           :ini t)))
  (values))

(defun add-radio (radio)
  "Adds passed RADIO (a PLIST) to QML item model.
  The model keys are:
  :name :hw-model :battery-level :current"
  (qjs |addRadio| ui:*radios* radio))

(defun clear ()
  (setf lora:*schedule-clear* nil)
  (q! |clear| ui:*radios*))

(defun change-radio (name) ; see QML
  (app:update-current-device name)
  (qlater (lambda () (lora:start-device-discovery name)))
  (values))

(defun reset-default-radio ()
  (app:change-setting :device nil)
  (lora:start-device-discovery))

