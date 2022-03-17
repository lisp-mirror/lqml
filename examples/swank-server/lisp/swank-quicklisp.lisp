;;; enable Swank and Quicklisp on mobile

(in-package :qml)

#+(and (or android ios) (not interpreter))
(ffi:clines "extern void init_lib_ASDF(cl_object);")

(defun %sym (symbol package)
  (intern (symbol-name symbol) package))

;;; Quicklisp setup

#+(or android ios)
(defun ensure-asdf ()
  (unless (find-package :asdf)
    (ffi:c-inline nil nil :void "ecl_init_module(NULL, init_lib_ASDF)" :one-liner t)
    (in-package :qml-user))
  :asdf)

#+(or android ios)
(defun quicklisp ()
  (ensure-asdf)
  (unless (find-package :quicklisp)
    #+android
    (progn
      (require :ecl-quicklisp)
      (require :deflate)
      (require :ql-minitar))
    #+ios
    (load "quicklisp/setup")
    ;; replace interpreted function with precompiled one from DEFLATE
    (setf (symbol-function (%sym 'gunzip :ql-gunzipper))
          (symbol-function (%sym 'gunzip :deflate)))
    (in-package :qml-user))
  :quicklisp)

;;; Swank setup

#+(or android ios)
(defun swank/create-server (interface port dont-close style)
  (funcall (%sym 'create-server :swank)
           :interface interface
           :port port
           :dont-close dont-close
           :style style))

#+(or android ios)
(defun start-swank (&key (port 4005) (interface "0.0.0.0") (style :spawn)
                         (load-contribs t) (setup t) (delete t) (quiet t)
                         (dont-close t) log-events)
  (unless (find-package :swank)
    (ensure-asdf)
    (funcall (%sym 'load-system :asdf) :swank))
  (funcall (%sym 'init :swank-loader)
           :load-contribs load-contribs
           :setup         setup
           :delete        delete
           :quiet         quiet)
  (setf (symbol-value (%sym '*log-events* :swank)) log-events)
  (eval (read-from-string "(swank/backend:defimplementation swank/backend:lisp-implementation-program () \"org.lisp.ecl\")"))
  (if (eql :spawn style)
      (swank/create-server interface port dont-close style)
      (mp:process-run-function
       "SLIME-listener"
       (lambda () (swank/create-server interface port dont-close style)))))

#+(or android ios)
(defun stop-swank (&optional (port 4005))
  (when (find-package :swank)
    (funcall (%sym 'stop-server :swank) port)
    :stopped))

#+(or android ios)
(progn
  ;; be careful not to use :s, :q in your mobile app code
  ;; ios simulator note: wrap :s and :q in qrun* (would crash otherwise)
  (define-symbol-macro :s (start-swank))
  (define-symbol-macro :q (quicklisp)))

#+(or android ios)
(export (list #+ios
              'start-swank
              'stop-swank
              'quicklisp))
