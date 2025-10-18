;;; keep sorted to recognize eventual name clashes

(defpackage ui
  (:use :cl)
  (:export
   #:*d-debug-input*
   #:*d-debug-model*
   #:*d-debug-text*
   #:*main*))

(in-package :ui)

(defparameter *d-debug-input* "debug_input")
(defparameter *d-debug-model* "debug_model")
(defparameter *d-debug-text*  "debug_text")

(defparameter *main* "main")
