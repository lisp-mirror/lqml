;;; this file will be loaded every time QML has been reloaded

(in-package :qml-user)

;; delay needed here because of initial reload
(qsingle-shot 1000 (lambda () (eval:eval-in-thread "(qml::help)"))) ; show help in REPL
