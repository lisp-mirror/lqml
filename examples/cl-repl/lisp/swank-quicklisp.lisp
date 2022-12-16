;;; enable Swank and Quicklisp on mobile

(in-package :qml)

#-interpreter
(ffi:clines "extern void init_lib_ASDF(cl_object);")

(defun %sym (symbol package)
  (intern (symbol-name symbol) package))

;;; Quicklisp setup

(defun ensure-asdf ()
  (unless (find-package :asdf)
    (ffi:c-inline nil nil :void "ecl_init_module(NULL, init_lib_ASDF)" :one-liner t)
    (in-package :qml-user))
  :asdf)

(defun quicklisp ()
  (ensure-asdf)
  (unless (find-package :quicklisp)
    #+android
    (require :ecl-quicklisp)
    #+ios
    (load "quicklisp/setup")
    ;; replace interpreted function with precompiled one from DEFLATE
    (setf (symbol-function (%sym 'gunzip :ql-gunzipper))
          (symbol-function (%sym 'gunzip :deflate)))
    (in-package :qml-user))
  :quicklisp)

;;; Swank setup

(defun swank/create-server (interface port dont-close style)
  (funcall (%sym 'create-server :swank)
           :interface interface
           :port port
           :dont-close dont-close
           :style style))

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
       (lambda () (swank/create-server interface port dont-close style))))
  ;; qrun*: only when running on main thread can we have return values from Qt
  (x:when-it (qrun* (qt:local-ip qt:*cpp*))
    (format nil "slime-connect ~A" x:it)))

(defun stop-swank (&optional (port 4005))
  (when (find-package :swank)
    (funcall (%sym 'stop-server :swank) port)
    :stopped))

(export
 (list 'start-swank
       'stop-swank
       'quicklisp))
