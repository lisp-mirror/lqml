;;; keep sorted to recognize eventual name clashes

(defpackage ui
  (:use :cl)
  (:export
   #:*busy*
   #:*loading*
   #:*main-view*
   #:*messages*
   #:*radios*))

(in-package :ui)

(defparameter *busy*      "busy")
(defparameter *loading*   "loading")
(defparameter *main-view* "main_view")
(defparameter *messages*  "messages")
(defparameter *radios*    "radios")

