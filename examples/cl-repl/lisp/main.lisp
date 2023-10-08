(in-package :editor)

#+(or android ios)
(when qml::*remote-ip*
  (qsingle-shot 1000 'auto-reload-qml))
