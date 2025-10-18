(defsystem :app
  :serial t
  :depends-on ()
  :components ((:file "lisp/package")
               (:file "lisp/ui-vars")
               (:file "lisp/d-dialogs")    ; for debug-ui
               (:file "lisp/d-input-hook") ; for debug-ui
               (:file "lisp/d-debug-ui")   ; for debug-ui
               (:file "lisp/main")))

