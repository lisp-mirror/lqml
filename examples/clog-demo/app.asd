(defsystem :app
  :serial t
  ;; requires this CLOG fork: https://gitlab.com/eql/clog-for-mobile/-/blob/main/clog-2.2.tgz
  :depends-on (#-depends-loaded :clog)
  :components ((:file "lisp/package")
               (:file "lisp/ini")
               (:file "lisp/ui-vars")
               #+mobile (:file "lisp/swank-quicklisp")
               (:file "lisp/eval")
               (:file "lisp/clog-bridge")
               (:file "clog-assets/demos/01-demo")
               (:file "lisp/main")))
