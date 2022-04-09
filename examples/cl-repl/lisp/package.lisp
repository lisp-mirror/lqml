(defpackage :editor
  (:nicknames :ed)
  (:use :cl :qml)
  (:export
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
   #:start))

