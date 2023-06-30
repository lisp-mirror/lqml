;;; keep sorted to recognize eventual name clashes

(defpackage ui
  (:use :cl)
  (:export
   #:*busy*
   #:*group*
   #:*loading*
   #:*main-view*
   #:*messages*
   #:*message-view*
   #:*modem*
   #:*radios*
   #:*region*
   #:*toast*
   #:*unread-messages*))

(in-package :ui)

(defparameter *busy*            "busy")
(defparameter *group*           "group")
(defparameter *loading*         "loading")
(defparameter *main-view*       "main_view")
(defparameter *messages*        "messages")
(defparameter *message-view*    "message_view")
(defparameter *modem*           "modem")
(defparameter *radios*          "radios")
(defparameter *region*          "region")
(defparameter *toast*           "toast")
(defparameter *unread-messages* "unread_messages")

