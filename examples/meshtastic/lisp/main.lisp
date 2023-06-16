(in-package :app)

;; set here the 4 character short name of your 2 devices
;; (see debug output during device discovery)

(defvar *device-1* "128c")
(defvar *device-2* "1c9c")

(defun ini ()
  (qt:ini)
  (qt:start-device-discovery qt:*ble* *device-1*) ; set device (see above)
  (msg:load-messages)
  (q> |playing| ui:*loading* nil) ; shown during Lisp startup
  (q> |playing| ui:*busy* t))     ; shown during BLE setup

(qlater 'ini)
