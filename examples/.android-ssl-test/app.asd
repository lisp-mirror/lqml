(defsystem :app
  :serial t
  :depends-on (#-depends-loaded :drakma)
  :components ((:file "lisp/package")
               (:file "lisp/ui-vars")
               (:file "lisp/main")))

