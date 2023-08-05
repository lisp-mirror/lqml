(defsystem :app
  :serial t
  :depends-on (#-depends-loaded :cl-base64
               #-depends-loaded :my-cl-protobufs
               #-depends-loaded :trivial-package-local-nicknames
               #+mobile :s-http-server
               #+mobile :zip) ; see 'hacks/zip/'
  :components ((:file "lisp/package")
               (:file "lisp/qt")
               (:file "lisp/ui-vars")
               #+mobile
               (:file "lisp/swank-quicklisp")
               #+mobile
               (:file "lisp/upload-download")
               (:file "lisp/db")
               (:file "lisp/group")
               (:file "lisp/messages")
               (:file "lisp/radios")
               (:file "lisp/lora")
               (:file "lisp/location")
               (:file "lisp/main")))

