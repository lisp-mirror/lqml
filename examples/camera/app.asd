(defsystem :app
  :serial t
  :depends-on (#-depends-loaded :s-http-server)
  :components ((:file "lisp/package")
               (:file "lisp/qt")
               (:file "lisp/web-server")
               (:file "lisp/main")))

