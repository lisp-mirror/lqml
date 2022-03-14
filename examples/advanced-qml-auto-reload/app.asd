(defsystem :app
  :serial t
  :depends-on ()
  :components ((:file "lisp/package")
               (:file "lisp/ui-vars")
               (:file "lisp/swank-quicklisp")
               (:file "lisp/eval")
               (:file "lisp/qml-reload/auto-reload-mobile")
               (:file "lisp/curl")
               (:file "lisp/main")))

