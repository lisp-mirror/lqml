;;; cross-compile ASDF system, using the byte-codes compiler
;;; as an intermediate step

;; optional vars, to be set in 'make.lisp' in your app dir
(defvar *epilogue-code* nil)
(defvar *ql-libs*       nil)

;;; *** (1) byte-compile ASDF system ***

(ext:install-bytecodes-compiler)

(when *ql-libs*
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
  (load *ql-libs*))

;;; load ASDF system and collect file names

(defvar *source-files* nil)

(defmethod asdf:perform ((o asdf:load-op) (c asdf:cl-source-file))
  (let ((source (namestring (asdf:component-pathname c))))
    (push (subseq source 0 (position #\. source :from-end t))
          *source-files*))
  (asdf::perform-lisp-load-fasl o c))

(asdf:load-system *asdf-system*)

(setf *source-files* (nreverse *source-files*))

;;; *** (2) cross-compile ***

;;; load and prepare cross-compiler

(ext:install-c-compiler)

(defun cc (&rest args)
  (apply 'concatenate 'string args))

#+(or android ios)
(load (merge-pathnames (format nil "platforms/~A/cross-compile"
                               #+android "android"
                               #+ios     "ios")))

(setf *load-verbose*    nil
      *compile-verbose* t)

(setf c::*suppress-compiler-warnings* nil
      c::*suppress-compiler-notes*    nil
      c::*compile-in-constants*       t)

;;(setf c::*compile-print* t) ; for debugging compile errors

(load (merge-pathnames "src/lisp/tr.lisp")) ; i18n

(setf *break-on-signals* 'error)

;;; compile/link manually (byte-compiled version is already loaded)

(defvar *object-files* nil)

(dolist (file *source-files*)
  (let ((src (cc file ".lisp"))
        (obj (merge-pathnames (format nil "src/.cache/ecl~A-~A/~A.o"
                                      (lisp-implementation-version)
                                      *architecture*
                                      file))))
    (when (or (not (probe-file obj))
              (> (file-write-date src)
                 (file-write-date obj)))
      (ensure-directories-exist obj)
      (compile-file src :output-file obj :system-p t))
    (push obj *object-files*)))

(setf *object-files* (nreverse *object-files*))

(c:build-static-library *library-name*
                        :lisp-files    *object-files*
                        :init-name     *init-name*
                        :epilogue-code *epilogue-code*)

