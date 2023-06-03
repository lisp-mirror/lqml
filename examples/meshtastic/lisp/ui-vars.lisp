;;; keep sorted to recognize eventual name clashes

(defpackage ui
  (:use :cl)
  (:export
   #:*busy*
   #:*hour-glass*
   #:*messages*
   #:*view*))

(in-package :ui)

(defparameter *busy*       "busy")
(defparameter *hour-glass* "hour_glass")
(defparameter *messages*   "messages")
(defparameter *view*       "view")

