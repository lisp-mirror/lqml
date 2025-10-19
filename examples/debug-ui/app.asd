(defsystem :app
  :serial t
  :depends-on (lqml-debug)
  :components ((:file "lisp/package")
               (:file "lisp/ui-vars")
               (:file "lisp/main")))

