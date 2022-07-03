(defpackage ui
  (:use :cl :qml)
  (:export
   #:*accuracy*
   #:*distance*
   #:*speed*))

(in-package :ui)

(defparameter *accuracy* "accuracy")
(defparameter *distance* "distance")
(defparameter *speed*    "speed")
