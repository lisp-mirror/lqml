(defsystem :app
  :serial t
  :depends-on ()
  :components ((:file "lisp/package")
               (:file "lisp/ui-vars")
               (:file "lisp/kalman")
               (:file "lisp/distance")
               (:file "lisp/speed")
               (:file "lisp/qt")
               (:file "lisp/main")))
