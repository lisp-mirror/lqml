;;; this file will be loaded every time QML has been reloaded

(in-package :editor)

(when (q< |small| ui:*main*)
  (change-font :smaller 3))
