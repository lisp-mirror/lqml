;;; enable Swank and Quicklisp on mobile

(in-package :qml)

;; for mobile app updates:
;; to be incremented on every ECL upgrade in order to replace all asset files
#+(or android ios)
(defconstant +app-version+ 1)

#+(and ios (not interpreter))
(ffi:clines "extern void init_lib_ASDF(cl_object);")

#+(or android ios)
(defvar *assets* #+android "assets:/lib/"
                 #+ios     "assets/")

#+ios
(defvar *bundle-root* (namestring *default-pathname-defaults*))

#+(or android ios)
(defun copy-asset-files (&optional (dir-name *assets*) origin)
  "Copy asset files to home directory."
  (flet ((directory-p (path)
           (x:ends-with "/" path))
         (translate (name)
           #+android
           (if (x:starts-with *assets* name)
               (subseq name (length *assets*))
               name)
           #+ios
           (namestring
	    (merge-pathnames (x:cc "../" (subseq name (length origin)))))))
    (ensure-directories-exist (translate dir-name))
    ;; note: both QDIRECTORY and QCOPY-FILE are prepared for accessing
    ;; APK asset files, which can't be accessed directly from Lisp
    (dolist (from (qdirectory dir-name))
      (if (directory-p from)
          (copy-asset-files from origin)
          (let ((to (translate from)))
            (when (probe-file to)
              (delete-file to))
            (unless (qcopy-file from to)
              (qlog "Error copying asset file: ~S" from)
              (return-from copy-asset-files))))))
  t)

#+(or android ios)
(let ((file ".app-version"))
  (defun app-version ()
    (if (probe-file file)
        (with-open-file (s file)
          (let ((str (make-string (file-length s))))
            (read-sequence str s)
            (values (parse-integer str))))
        0))
  (defun save-app-version ()
    (with-open-file (s file :direction :output :if-exists :supersede)
      (princ +app-version+ s))
    (values)))

(defun %sym (symbol package)
  (intern (symbol-name symbol) package))

;;; Quicklisp setup

#+ios
(defun load-asdf ()
  (unless (find-package :asdf)
    ;; needed for ASDF and Quicklisp
    (setf (logical-pathname-translations "SYS")
          (list (list "sys:**;*.*"
                      (merge-pathnames "**/*.*" (user-homedir-pathname)))
                (list "home:**;*.*"
                      (merge-pathnames "**/*.*" (user-homedir-pathname)))))
    (ffi:c-inline nil nil :void "ecl_init_module(NULL, init_lib_ASDF)" :one-liner t)
    (in-package :qml-user))
  :asdf)

#+(or android ios)
(defun ensure-asdf ()
  (unless (find-package :asdf)
    #+android
    (require :asdf)
    #+ios
    (load-asdf)))

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
              'load-asdf
              'start-swank
              'stop-swank
              'quicklisp))

#+ios
(progn
  ;; adapt paths to iOS specific values
  (defvar *user-homedir-pathname-orig* (symbol-function 'user-homedir-pathname))

  (ext:package-lock :common-lisp nil)

  (defun cl:user-homedir-pathname (&optional host)
    (merge-pathnames "Library/" (funcall *user-homedir-pathname-orig* host)))

  (ext:package-lock :common-lisp t)

  (dolist (el '(("XDG_DATA_HOME"   . "")
                ("XDG_CONFIG_HOME" . "")
                ("XDG_DATA_DIRS"   . "")
                ("XDG_CONFIG_DIRS" . "")
                ("XDG_CACHE_HOME"  . ".cache")))
    (ext:setenv (car el) (namestring (merge-pathnames (cdr el)
                                                      (user-homedir-pathname))))))

;;; ini

#+(or android ios)
(defun startup-ini ()
  #+ios
  (setf *default-pathname-defaults* (user-homedir-pathname))
  (ext:install-bytecodes-compiler)
  (and (/= +app-version+ (app-version))
       #+ios
       (let ((dir (namestring (merge-pathnames *assets* *bundle-root*))))
         (copy-asset-files dir dir))
       #+android
       (copy-asset-files)
       (save-app-version)))

#+(or android ios)
(qlater 'startup-ini)
