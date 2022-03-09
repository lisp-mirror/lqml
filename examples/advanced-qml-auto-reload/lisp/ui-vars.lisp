(defpackage ui
  (:use :cl :qml)
  (:export
   #:*flick-output*
   #:*history-back*
   #:*history-forward*
   #:*progress*
   #:*repl-input*
   #:*repl-output*
   #:*repl-model*))

(in-package :ui)

(defparameter *flick-output*    "flick_output")
(defparameter *history-back*    "history_back")
(defparameter *history-forward* "history_forward")
(defparameter *progress*        "progress")
(defparameter *repl-input*      "repl_input")
(defparameter *repl-output*     "repl_output")
(defparameter *repl-model*      "repl_model")
