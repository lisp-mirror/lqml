(defpackage :qt
  (:use :cl :qml)
  (:export
   #:*cpp*
   #:ini
   ;; new
   #:make-object
   ;; connect
   #:connect-document-changed
   #+ios
   #:connect-key-pressed
   ;; methods
   #:block*
   #:block-number
   #:character-at
   #:clear-undo-redo-stacks
   #:current-block-state
   #:find*
   #:find-block-by-line-number
   #:exact-match
   #:line-count
   #:local-ip
   #:next
   #:position*
   #:position-in-block
   #:previous
   #:previous-block-state
   #:set-current-block-state
   #:set-format
   #:set-pattern
   #:text
   #:text-document
   ;; ulisp (e.g. Arduino)
   #+linux #:connect-usb
   #+linux #:send-to-ulisp))

(in-package :qt)

(defvar *cpp* nil)

(defun ini ()
  (setf *cpp*
        #+qt-plugin (qload-c++ "cpp/qt")
        #-qt-plugin (qfind-child nil "QT"))
  (let ((*package* (find-package :qt)))
    (define-qt-wrappers *cpp*)))
