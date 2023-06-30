(in-package :qml-user)

(pushnew :qt-plugin *features*)

(require :asdf)

(push (merge-pathnames "./")
      asdf:*central-registry*)

(asdf:load-system :uiop)
(asdf:load-system :cl-base64)
(asdf:load-system :sqlite)
(asdf:load-system :trivial-package-local-nicknames)

;; may take very long on mobile devices
(time (asdf:load-system :my-cl-protobufs))

(push :depends-loaded *features*)

(asdf:operate 'asdf:load-source-op :app)

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
  (load "~/slime/lqml-start-swank") ; for 'slime-connect' from Emacs
  (qlater (lambda () (in-package :lora))))

