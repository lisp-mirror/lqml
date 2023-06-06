;;; keep sorted to recognize eventual name clashes

(defpackage ui
  (:use :cl)
  (:export
   #:*busy*
   #:*loading*
   #:*messages*
   #:*view*))

(in-package :ui)

(defparameter *busy*     "busy")
(defparameter *loading*  "loading")
(defparameter *messages* "messages")
(defparameter *view*     "view")

