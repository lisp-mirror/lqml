(defpackage :qml
  (:use :cl)
  (:export
   #:*break-on-errors*
   #:*quick-view*
   #:*engine*
   #:*root-item*
   #:*caller*
   #:clipboard-text
   #:copy-all-asset-files
   #:define-qt-wrappers
   #:disable-clipboard-menu
   #:ensure-permissions
   #:find-quick-item
   #:pixel-ratio
   #:qapropos
   #:qapropos*
   #:qchildren
   #:qcopy-file
   #:qdirectory
   #:qeql
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
   #:qnull
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
   #:qt-object
   #:qt-object-p
   #:reload
   #:root-item
   #:set-clipboard-text
   #:tr
   #:view-status-changed
   #:!))

(defpackage :qml-user
  (:use :cl :qml))

(pushnew :lqml *features*)
