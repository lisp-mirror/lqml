;;; keep sorted to recognize eventual name clashes

(defpackage ui
  (:use :cl)
  (:export
   #:*add-default-marker*
   #:*busy*
   #:*dialogs*
   #:*dialog-spin-box*
   #:*edit*
   #:*emojis*
   #:*find*
   #:*find-text*
   #:*group*
   #:*group-icon*
   #:*group-view*
   #:*hourglass*
   #:*location*
   #:*main-view*
   #:*map*
   #:*map-loader*
   #:*map-view*
   #:*markers*
   #:*messages*
   #:*message-view*
   #:*modem*
   #:*position-source*
   #:*radio-icon*
   #:*radios*
   #:*recent-emojis*
   #:*region*
   #:*remove-marker*
   #:*toast*
   #:*unread-messages*))

(in-package :ui)

(defparameter *add-default-marker* "add_default_marker")
(defparameter *busy*               "busy")
(defparameter *dialogs*            "dialogs")
(defparameter *dialog-spin-box*    "dialog_spin_box")
(defparameter *edit*               "edit")
(defparameter *emojis*             "emojis")
(defparameter *find*               "find")
(defparameter *find-text*          "find_text")
(defparameter *group*              "group")
(defparameter *group-icon*         "group_icon")
(defparameter *group-view*         "group_view")
(defparameter *hourglass*          "hourglass")
(defparameter *location*           "location")
(defparameter *map*                "map")
(defparameter *map-loader*         "map_loader")
(defparameter *map-view*           "map_view")
(defparameter *markers*            "markers")
(defparameter *main-view*          "main_view")
(defparameter *messages*           "messages")
(defparameter *message-view*       "message_view")
(defparameter *modem*              "modem")
(defparameter *position-source*    "position_source")
(defparameter *radio-icon*         "radio_icon")
(defparameter *radios*             "radios")
(defparameter *recent-emojis*      "recent_emojis")
(defparameter *region*             "region")
(defparameter *remove-marker*      "remove_marker")
(defparameter *toast*              "toast")
(defparameter *unread-messages*    "unread_messages")

