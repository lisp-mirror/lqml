(defpackage ui
  (:use :cl :qml)
  (:export
   #:*board*
   #:*buttons1*
   #:*buttons2*
   #:*down*
   #:*dynamic*
   #:*left*
   #:*level*
   #:*next*
   #:*previous*
   #:*restart*
   #:*right*
   #:*rotate-player*
   #:*solve*
   #:*undo*
   #:*up*
   #:*wiggle-box*
   #:*zoom-board-in*
   #:*zoom-board-out*))

(in-package :ui)

(defparameter *board*          "board")
(defparameter *buttons1*       "buttons1")
(defparameter *buttons2*       "buttons2")
(defparameter *down*           "down")
(defparameter *dynamic*        "dynamic")
(defparameter *left*           "left")
(defparameter *level*          "level")
(defparameter *next*           "next")
(defparameter *previous*       "previous")
(defparameter *restart*        "restart")
(defparameter *right*          "right")
(defparameter *rotate-player*  "rotate_player")
(defparameter *solve*          "solve")
(defparameter *undo*           "undo")
(defparameter *up*             "up")
(defparameter *wiggle-box*     "wiggle_box")
(defparameter *zoom-board-in*  "zoom_board_in")
(defparameter *zoom-board-out* "zoom_board_out")
