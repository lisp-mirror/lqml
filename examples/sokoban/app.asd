(defsystem :app
  :serial t
  :depends-on ()
  :components ((:file "lisp/3rd-party/sokoban")
               (:file "lisp/3rd-party/my-levels")
               (:file "lisp/package")
               (:file "lisp/ui-vars")
               (:file "lisp/sokoban")))
