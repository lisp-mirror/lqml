(in-package :editor)

(ignore-errors ; don't hang on startup
 (load (merge-pathnames ".eclrc")))

#+(or android ios)
(when qml::*remote-ip*
  (qsingle-shot 1000 'auto-reload-qml))
