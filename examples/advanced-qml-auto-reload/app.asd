(defsystem :app
  :serial t
  :depends-on ()
  :components ((:file "lisp/package")
               (:file "lisp/ui-vars")
               #+mobile
               (:file "lisp/swank-quicklisp")
               (:file "lisp/eval")
               #+mobile
               (:file "lisp/qml-reload/auto-reload-mobile")
               (:file "lisp/curl")
               (:file "lisp/main")))

