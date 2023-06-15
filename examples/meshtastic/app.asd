(defsystem :app
  :serial t
  :depends-on (#-depends-loaded :uiop
               #-depends-loaded :cl-base64
               #-depends-loaded :my-cl-protobufs
               #-depends-loaded :trivial-package-local-nicknames)
  :components ((:file "lisp/package")
               (:file "lisp/qt")
               (:file "lisp/ui-vars")
               (:file "lisp/radio")
               (:file "lisp/messages")
               (:file "lisp/main")))

