(defpackage ui
  (:use :cl :qml)
  (:export
   #:*accuracy*
   #:*distance*
   #:*max-speed*
   #:*position-source*
   #:*speed*))

(in-package :ui)

(defparameter *accuracy*        "accuracy")
(defparameter *distance*        "distance")
(defparameter *max-speed*       "max_speed")
(defparameter *position-source* "position_source")
(defparameter *speed*           "speed")
