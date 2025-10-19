;;; keep sorted to recognize eventual name clashes

(defpackage ui
  (:use :cl)
  (:export
   #:*main*))

(in-package :ui)

(defparameter *main* "main")
