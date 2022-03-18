;;; cross-compile ASDF system, using the byte-codes compiler as an intermediate
;;; step; this requires a small ASDF hack, but won't even try to build useless
;;; cross-compiled *.fas files -- we load byte-compiled *.fasc files instead
;;;
;;; this is both faster and avoids a 'ld' error when cross-compiling for iOS
;;; (unknown flags for static build)
;;;
;;; tested with quite a few Quicklisp libs

(in-package :cl-user)

;; optional, to be set in 'make.lisp' in your app dir
(defvar *ql-libs* nil)
(defvar *require* nil)

(dolist (lib *require*)
  (require lib))

(defun cc (&rest args)
  (apply 'concatenate 'string args))

(let* ((cache (namestring asdf:*user-cache*))
       (p (search "/ecl" cache)))
  (setf asdf:*user-cache*
        (pathname (cc (subseq cache 0 p)
                      "/ecl-" #+android "android" #+ios "ios"
                      (subseq cache (+ 4 p))))))

;;; *** (1) byte-compile ASDF system ***

(ext:install-bytecodes-compiler)

(when *ql-libs*
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))
  (load *ql-libs*)) ; eventual, not yet installed dependencies

;;; load ASDF system

(asdf:load-system *asdf-system*)

;;; *** (2) cross-compile ***

;;; load and prepare cross-compiler

(ext:install-c-compiler)

(setf *features* (remove :interpreter *features*))

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

;;; --- HACK begin ---

(in-package :asdf/lisp-action)

(defmethod asdf:perform ((o asdf:load-op) (c asdf:cl-source-file))
  (if-let (fasl (first (input-files o c)))
    ;; load above compiled *.fasc instead of cross-compiled *.fas
    (load (cl-user::cc (namestring fasl) "c"))))

(in-package :cl-user)

(defun c:build-fasl (file &rest _)
  ;; do nothing (not needed when cross-compiling)
  file)

;;; --- HACK end ---

(asdf:make-build *asdf-system*
                 :monolithic t
                 :type       :static-library
                 :move-here  *library-path*
                 :init-name  *init-name*)
