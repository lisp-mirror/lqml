(defsystem :app
  :serial t
  :depends-on ()
  :components ((:file "lisp/package")
               (:file "lisp/qt")
               (:file "lisp/ui-vars")
               (:file "lisp/db")
               (:file "lisp/main")))

