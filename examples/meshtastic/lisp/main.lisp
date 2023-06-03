(in-package :app)

(defun ini ()
  (qt:ini)
  (msg:load-messages)
  (q> |visible| ui:*hour-glass* nil) ; shown during Lisp startup
  (q> |playing| ui:*busy* t))        ; shown during BLE setup

(qlater 'ini)
