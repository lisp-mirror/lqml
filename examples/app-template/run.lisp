(in-package :qml-user)

(require :asdf)

(push (merge-pathnames "./")
      asdf:*central-registry*)

(asdf:operate 'asdf:load-source-op :app)

(qset *quick-view*
      |x| 75
      |y| 75)

;;; for Slime after copying 'lqml-start-swank.lisp' from LQML sources
;;; to your Slime directory, which is assumed to be '~/slime/'

(when (find "-slime" (ext:command-args) :test 'string=)
  (load "~/slime/lqml-start-swank")) ; for 'slime-connect' from Emacs

