(defsystem :app
  :serial t
  :depends-on ()
  :components ((:file "lisp/package")
               (:file "lisp/ui-vars")
               (:file "lisp/maze")
               (:file "lisp/main")))

