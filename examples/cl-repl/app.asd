(defsystem :app
  :serial t
  :depends-on (#-:depends-loaded :cl-ppcre
               #-:depends-loaded :s-http-server
               #-:depends-loaded :zip)
  :components ((:file "lisp/package")
               (:file "lisp/ui-vars")
               (:file "lisp/qt")
               #+mobile
               (:file "lisp/swank-quicklisp")
               #+mobile
               (:file "lisp/qml-reload/auto-reload-mobile")
               (:file "lisp/data/lisp-keywords")
               (:file "lisp/data/lqml-keywords")
               (:file "lisp/data/keywords")
               (:file "lisp/input-hook")
               (:file "lisp/top-level")
               (:file "lisp/eval")
               (:file "lisp/curl")
               (:file "lisp/dialogs")
               (:file "lisp/editor")
               (:file "lisp/upload-download")
               (:file "lisp/ini")
               (:file "lisp/main")))

