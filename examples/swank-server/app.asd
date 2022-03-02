(defsystem :app
  :serial t
  :depends-on ()
  :components ((:file "lisp/package")
               (:file "lisp/ui-vars")
               (:file "lisp/swank-quicklisp")
               (:file "lisp/eval")
               (:file "lisp/main")))

