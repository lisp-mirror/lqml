(defpackage ui
  (:use :cl :qml)
  (:export
   #:*ball*
   #:*maze*
   #:*timer*))

(in-package :ui)

(defparameter *ball*  "ball")
(defparameter *maze*  "maze")
(defparameter *timer* "timer")

