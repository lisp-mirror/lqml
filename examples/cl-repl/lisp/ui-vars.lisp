;;; keep sorted to recognize eventual name clashes

(defpackage ui
  (:use :cl)
  (:export
   #:*buttons-right*
   #:*buttons-top*
   #:*clear*
   #:*clipboard-menu*
   #:*command*
   #:*copy*
   #:*cursor-color*
   #:*cut*
   #:*debug-dialog*
   #:*debug-input*
   #:*debug-model*
   #:*debug-text*
   #:*dialogs*
   #:*dynamic*
   #:*down*
   #:*edit*
   #:*eval*
   #:*eval-exp*
   #:*file-browser*
   #:*file-edit*
   #:*flick-command*
   #:*flick-edit*
   #:*folder-model*
   #:*folder-view*
   #:*font-bigger*
   #:*font-smaller*
   #:*history-back*
   #:*history-forward*
   #:*left*
   #:*main*
   #:*menu-timer*
   #:*open-file*
   #:*output*
   #:*output-model*
   #:*paren-close*
   #:*paren-open*
   #:*paste*
   #:*path*
   #:*progress*
   #:*query-dialog*
   #:*query-input*
   #:*query-text*
   #:*rect-arrows*
   #:*rect-command*
   #:*rect-edit*
   #:*rect-output*
   #:*rect-paren-buttons*
   #:*redo*
   #:*right*
   #:*save-file*
   #:*select-all*
   #:*show-buttons-right*
   #:*show-buttons-top*
   #:*show-menu*
   #:*undo*
   #:*up*))

(in-package :ui)

(defparameter *buttons-right*      "buttons_right")
(defparameter *buttons-top*        "buttons_top")
(defparameter *clear*              "clear")
(defparameter *clipboard-menu*     "clipboard_menu")
(defparameter *command*            "command")
(defparameter *copy*               "copy")
(defparameter *cursor-color*       "cursor_color")
(defparameter *cut*                "cut")
(defparameter *debug-dialog*       "debug_dialog")
(defparameter *debug-input*        "debug_input")
(defparameter *debug-model*        "debug_model")
(defparameter *debug-text*         "debug_text")
(defparameter *dialogs*            "dialogs")
(defparameter *dynamic*            "dynamic")
(defparameter *down*               "down")
(defparameter *edit*               "edit")
(defparameter *eval*               "eval")
(defparameter *eval-exp*           "eval_exp")
(defparameter *file-browser*       "file_browser")
(defparameter *file-edit*          "file_edit")
(defparameter *flick-command*      "flick_command")
(defparameter *flick-edit*         "flick_edit")
(defparameter *folder-model*       "folder_model")
(defparameter *folder-view*        "folder_view")
(defparameter *font-bigger*        "font_bigger")
(defparameter *font-smaller*       "font_smaller")
(defparameter *history-back*       "history_back")
(defparameter *history-forward*    "history_forward")
(defparameter *left*               "left")
(defparameter *main*               "main")
(defparameter *menu-timer*         "menu_timer")
(defparameter *open-file*          "open_file")
(defparameter *output-model*       "output_model")
(defparameter *output*             "output")
(defparameter *paren-close*        "paren_close")
(defparameter *paren-open*         "paren_open")
(defparameter *paste*              "paste")
(defparameter *path*               "path")
(defparameter *progress*           "progress")
(defparameter *query-dialog*       "query_dialog")
(defparameter *query-input*        "query_input")
(defparameter *query-text*         "query_text")
(defparameter *rect-arrows*        "rect_arrows")
(defparameter *rect-command*       "rect_command")
(defparameter *rect-edit*          "rect_edit")
(defparameter *rect-output*        "rect_output")
(defparameter *rect-paren-buttons* "rect_paren_buttons")
(defparameter *redo*               "redo")
(defparameter *right*              "right")
(defparameter *save-file*          "save_file")
(defparameter *select-all*         "select_all")
(defparameter *show-buttons-right* "show_buttons_right")
(defparameter *show-buttons-top*   "show_buttons_top")
(defparameter *show-menu*          "show_menu")
(defparameter *undo*               "undo")
(defparameter *up*                 "up")
