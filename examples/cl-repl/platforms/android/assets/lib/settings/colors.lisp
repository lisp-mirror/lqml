(in-package :editor)

(setf *text-color*              "black")
(setf *background-color*        "white")
(setf *selected-text-color*     "white")
(setf *selection-color*         "firebrick")
(setf *parenthesis-color*       "lightslategray")
(setf *string-color*            "saddlebrown")
(setf *comment-color*           "lightslategray")
(setf *lisp-keyword-color*      "#c05050")
(setf *lqml-keyword-color*      "#5050c0")
(setf *keyword-color*           "#409090")

(setf *output-text-color*       "black")
(setf *output-background-color* "lavender")
(setf *output-string-color*     "saddlebrown")
(setf *output-value-color*      "#2020ff")
(setf *output-trace-color*      "darkmagenta")
(setf *output-error-color*      "red")

(setf *button-color*            "#e0e0e0")
(setf *button-text-color*       "#26282a")
(setf *button-opacity*          0.12) ; arrow and paren buttons only

(setf *cursor-color*            "blue")

(progn
  (qrun* (apply-colors))
  (q! |clear| ui:*output-model*)
  (values))

