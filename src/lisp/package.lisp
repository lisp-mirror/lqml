(defpackage :qml
  (:use :common-lisp)
  (:export
   #:*break-on-errors*
   #:*quick-view*
   #:*root-item*
   #:*caller*
   #:children
   #:define-qt-wrappers
   #:find-quick-item
   #:js
   #:make-qobject
   #:pixel-ratio
   #:qapropos
   #:qapropos*
   #:qfind-child
   #:qml-call
   #:qml-get
   #:qml-set
   #:qml-set-all
   #:q!
   #:q<
   #:q>
   #:q>*
   #:qjs
   #:qchildren
   #:qescape
   #:qexec
   #:qexit
   #:qfrom-utf8
   #:qfun
   #:qget
   #:qset
   #:qlater
   #:qload-c++
   #:qlog
   #:qobject-p
   #:qprocess-events
   #:qq
   #:qquit
   #:qrun
   #:qrun-on-ui-thread
   #:qrun*
   #:qrun-on-ui-thread*
   #:qsingle-shot
   #:qsleep
   #:qtranslate
   #:qversion
   #:reload
   #:root-item
   #:tr
   #:!))

(defpackage :qml-user
  (:use :common-lisp :qml))

(pushnew :qml *features*)
