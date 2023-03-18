(defsystem :app
  :serial t
  :depends-on (#-depends-loaded :cl-ppcre
               #+(or mobile (not depends-loaded)) :s-http-server
               #+(or mobile (not depends-loaded)) :zip) ; see 'patch/'
  :components ((:file "lisp/package")
               (:file "lisp/ui-vars")
               (:file "lisp/qt")
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
               (:file "lisp/qml-item")
               (:file "lisp/editor")
               (:file "lisp/ini")
               #+mobile
               (:file "lisp/swank-quicklisp")
               #+mobile
               (:file "lisp/upload-download")
               (:file "lisp/main")))

