(defpackage ui
  (:use :cl)
  (:export
   #:*accuracy*
   #:*animation*
   #:*heart-rate*
   #:*main*
   #:*zoom-in*
   #:*zoom-out*))

(in-package :ui)

(defparameter *animation*  "animation")
(defparameter *accuracy*   "accuracy")
(defparameter *heart-rate* "heart_rate")
(defparameter *main*       "main")
(defparameter *zoom-in*    "zoom_in")
(defparameter *zoom-out*   "zoom_out")
