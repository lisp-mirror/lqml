;;; keep sorted to recognize eventual name clashes

(defpackage ui
  (:use :cl)
  (:export
   #:*add-manual-marker*
   #:*busy*
   #:*channel-name*
   #:*dialogs*
   #:*dialog-line-edit*
   #:*dialog-spin-box*
   #:*edit*
   #:*emojis*
   #:*find*
   #:*find-text*
   #:*group*
   #:*group-icon*
   #:*group-view*
   #:*help*
   #:*hourglass*
   #:*location*
   #:*main-view*
   #:*map*
   #:*map-loader*
   #:*map-view*
   #:*markers*
   #:*menu*
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

(defparameter *add-manual-marker* "add_manual_marker")
(defparameter *busy*              "busy")
(defparameter *channel-name*      "channel_name")
(defparameter *dialogs*           "dialogs")
(defparameter *dialog-line-edit*  "dialog_line_edit")
(defparameter *dialog-spin-box*   "dialog_spin_box")
(defparameter *edit*              "edit")
(defparameter *emojis*            "emojis")
(defparameter *find*              "find")
(defparameter *find-text*         "find_text")
(defparameter *group*             "group")
(defparameter *group-icon*        "group_icon")
(defparameter *group-view*        "group_view")
(defparameter *help*              "help")
(defparameter *hourglass*         "hourglass")
(defparameter *location*          "location")
(defparameter *map*               "map")
(defparameter *map-loader*        "map_loader")
(defparameter *map-view*          "map_view")
(defparameter *markers*           "markers")
(defparameter *main-view*         "main_view")
(defparameter *menu*              "menu")
(defparameter *messages*          "messages")
(defparameter *message-view*      "message_view")
(defparameter *modem*             "modem")
(defparameter *position-source*   "position_source")
(defparameter *radio-icon*        "radio_icon")
(defparameter *radios*            "radios")
(defparameter *recent-emojis*     "recent_emojis")
(defparameter *region*            "region")
(defparameter *remove-marker*     "remove_marker")
(defparameter *toast*             "toast")
(defparameter *unread-messages*   "unread_messages")

