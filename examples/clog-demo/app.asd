(defsystem :app
  :serial t
  :depends-on (#-clog-loaded :clog) ; requires this fork: https://github.com/pls153/clog
  :components ((:file "lisp/package")
               (:file "lisp/ui-vars")
               (:file "lisp/swank-quicklisp")
               (:file "lisp/eval")
               (:file "lisp/clog-bridge")
               (:file "clog-assets/demos/01-demo")
               (:file "lisp/main")))
