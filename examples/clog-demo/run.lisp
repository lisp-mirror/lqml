(in-package :qml-user)

(require :asdf)

(progn
  (pushnew :mobile *features*)
  (asdf:load-system :clog)
  (setf *features* (remove :mobile *features*)))

(push (merge-pathnames "./")
      asdf:*central-registry*)

(push :depends-loaded *features*)

(asdf:operate 'asdf:load-source-op :app)

(setf *features* (remove :mobile *features*))

(qset *quick-view*
      |x| 75
      |y| 75)

(defun option (name)
  (find name (ext:command-args) :test 'search))

;;; trivial auto reload of all QML files after saving any change

(when (option "-auto")
  (load "lisp/qml-reload/auto-reload"))

;;; for Slime after copying 'lqml-start-swank.lisp' from LQML sources
;;; to your Slime directory, which is assumed to be '~/slime/'

(when (option "-slime")
  (load "~/slime/lqml-start-swank")) ; for 'slime-connect' from Emacs
