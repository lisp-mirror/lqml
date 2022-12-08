(in-package :editor)

#+mobile
(when qml::*remote-ip*
  (qsingle-shot 1000 'auto-reload-qml))
