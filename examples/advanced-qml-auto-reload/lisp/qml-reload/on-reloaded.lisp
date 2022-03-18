;;; this file will be loaded every time 'main.qml' has been reloaded

(in-package :qml-user)

;;; delay needed here because of indirect QML Loader, needed for
;;; single file auto reload

(qsingle-shot 1000 (lambda () (eval:eval-in-thread "(qml::help)"))) ; show help in REPL
