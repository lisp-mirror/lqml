(in-package :qml-user)

(load "lisp/main")

(qset *quick-view*
      |x| 75
      |y| 75)

;;; for Slime after copying 'qml-start-swank.lisp' from LQML sources
;;; to your Slime directory, which is assumed to be '~/slime/'

(when (find "-slime" (ext:command-args) :test 'string=)
  (load "~/slime/qml-start-swank")) ; for 'slime-connect' from Emacs

