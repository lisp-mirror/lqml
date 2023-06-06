(in-package :app)

(defun ini ()
  (qt:ini)
  (qt:start-device-discovery qt:*ble*)
  (msg:load-messages)
  (q> |playing| ui:*loading* nil) ; shown during Lisp startup
  (q> |playing| ui:*busy* t))     ; shown during BLE setup

(qlater 'ini)
