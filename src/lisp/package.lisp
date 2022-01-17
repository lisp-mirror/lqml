(defpackage :qml
  (:use :common-lisp)
  (:export
   #:*break-on-errors*
   #:*quick-view*
   #:*root*
   #:*root-item*
   #:*caller*
   #:children
   #:find-quick-item
   #:js
   #:make-qobject
   #:pixel-ratio
   #:qapropos
   #:qapropos*
   #:qml-call
   #:qml-get
   #:qml-set
   #:qml-set-all
   #:q!
   #:q<
   #:q>
   #:q>*
   #:qjs
   #+linux
   #:qchild-items
   #:qescape
   #:qexec
   #:qexit
   #:qfind-child
   #:qfind-children
   #:qfrom-utf8
   #:qfun
   #:qget
   #:qset
   #:qlater
   #:qload-c++
   #:qlocal8bit
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
   #:qutf8
   #:qversion
   #:qvariant-from-value
   #:qvariant-value
   #:root-item
   #:reload
   #:tr))

(defpackage :qml-user
  (:use :common-lisp :qml))

(pushnew :qml *features*)
