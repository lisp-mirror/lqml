;;; keep sorted to recognize eventual name clashes

(defpackage ui
  (:use :cl)
  (:export
   #:*busy*
   #:*edit*
   #:*group*
   #:*group-icon*
   #:*group-view*
   #:*find*
   #:*find-text*
   #:*loading*
   #:*main-view*
   #:*messages*
   #:*message-view*
   #:*modem*
   #:*position-source*
   #:*radio-icon*
   #:*radios*
   #:*region*
   #:*toast*
   #:*unread-messages*))

(in-package :ui)

(defparameter *busy*            "busy")
(defparameter *edit*            "edit")
(defparameter *group*           "group")
(defparameter *group-icon*      "group_icon")
(defparameter *group-view*      "group_view")
(defparameter *find*            "find")
(defparameter *find-text*       "find_text")
(defparameter *loading*         "loading")
(defparameter *main-view*       "main_view")
(defparameter *messages*        "messages")
(defparameter *message-view*    "message_view")
(defparameter *modem*           "modem")
(defparameter *position-source* "position_source")
(defparameter *radio-icon*      "radio_icon")
(defparameter *radios*          "radios")
(defparameter *region*          "region")
(defparameter *toast*           "toast")
(defparameter *unread-messages* "unread_messages")

