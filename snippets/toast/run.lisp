(in-package :qml-user)

(defun toast (message)
  (qjs |message| "toast" message))

(qsingle-shot 1000 (lambda () (toast "You look tired, go get some coffee.")))

