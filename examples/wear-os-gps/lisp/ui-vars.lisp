(defpackage ui
  (:use :cl :qml)
  (:export
   #:*always-on*
   #:*accuracy*
   #:*distance*
   #:*max-speed*
   #:*position-source*))

(in-package :ui)

(defparameter *always-on*       "always_on")
(defparameter *accuracy*        "accuracy")
(defparameter *distance*        "distance")
(defparameter *max-speed*       "max_speed")
(defparameter *position-source* "position_source")
