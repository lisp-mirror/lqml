(in-package :app)

;; set here the name and address of your 2 devices
;; (see debug output during device discovery)

(defvar *device-1* '(:name "Meshtastic_128c" :address "F4:12:FA:9D:12:8D"))
(defvar *device-2* '(:name "Meshtastic_1c9c" :address "F4:12:FA:9D:1C:9D"))

(defun ini ()
  (qt:ini)
  (qt:start-device-discovery qt:*ble* *device-1*) ; set device (see above)
  (msg:load-messages)
  (q> |playing| ui:*loading* nil) ; shown during Lisp startup
  (q> |playing| ui:*busy* t))     ; shown during BLE setup

(qlater 'ini)
