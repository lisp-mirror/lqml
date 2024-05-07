(defpackage :editor
  (:nicknames :ed)
  (:use :cl :qml)
  (:export
   ;; editor
   #:*file*
   #:*plain-text-search*
   #:append-output
   #:change-font
   #:close-all-parens
   #:eval*
   #:ensure-focus
   #:find-text
   #:pr ; nick for 'append-output'
   #:reload-qml
   #:save-changes
   #:set-font
   #:start
   ;; uLisp mode (e.g. Arduino)
   #+linux #:*ulisp-mode*
   #+linux #:send-to-ulisp
   #+linux #:received-from-ulisp))

