(defsystem :app
  :serial t
  :depends-on ()
  :components ((:file "lisp/package")
               (:file "lisp/definitions")
               (:file "lisp/utils")
               (:file "lisp/main")))

