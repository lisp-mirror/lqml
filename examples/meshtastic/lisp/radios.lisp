(in-package :radios)

(defvar *connection* nil)
(defvar *found*      nil)

(defun ini ()
  (setf *connection* (or (app:setting :connection)
                         :ble))
  (set-connection-type)
  (q> |checked| (symbol-name *connection*) t)
  (q> |model| ui:*region*
      (cons "-" (mapcar 'symbol-name (rest (lora:keywords :region-code)))))
  (set-region)
  (x:when-it (app:setting :device-filter)
    (qt:set-device-filter qt:*cpp* x:it)))

(defun connection-changed (name)
  (when (eql *connection* :ble)
    (qt:stop-device-discovery qt:*cpp*))
  (let ((con (app:kw name)))
    (setf *connection* con)
    (app:change-setting :connection con))
  (set-connection-type)
  (lora:start-device-discovery))

(defun set-connection-type ()
  (qt:set-connection-type qt:*cpp* (symbol-name *connection*)))

(defun saved-region ()
  (let ((region (app:setting :region)))
    (unless (find region '(nil :unset))
      region)))

(defun set-region ()
  (x:when-it (saved-region)
    (q> |currentIndex| ui:*region*
        (q! |indexOfValue| ui:*region*
            (symbol-name x:it)))))

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

(defun reset ()
  (when (eql *connection* :ble)
    (lora:start-device-discovery)))

