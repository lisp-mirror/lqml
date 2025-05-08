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
(defvar *ql-libs*    nil)
(defvar *require*    nil)
(defvar *build-type* nil)
(defvar *init-name*  nil)

(pushnew :lqml *features*)

(dolist (lib *require*)
  (require lib))

(defun cc (&rest args)
  (apply 'concatenate 'string args))

(defvar *32bit* (<= most-positive-fixnum (expt 2 32)))

(let* ((cache (namestring asdf:*user-cache*))
       (p (search "/ecl" cache)))
  (setf asdf:*user-cache*
        (pathname (cc (subseq cache 0 p)
                      "/ecl-" #+android "android" #+ios "ios"
                      (if *32bit* "-32bit" "")
                      (subseq cache (+ 4 p))))))

;;; *** (1) byte-compile ASDF system ***

(ext:install-bytecodes-compiler)

(when (or *ql-libs*
          (eql :fasl *build-type*)) ; load Quicklisp for finding systems
  (let ((home (user-homedir-pathname)))
    #+ios
    (setf home (make-pathname :directory
                              (remove "Library" (pathname-directory home)
                                      :test 'string=)))
    (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" home)))
      (if (probe-file quicklisp-init)
          (load quicklisp-init)
          (require :ecl-quicklisp))))
  (when *ql-libs*
    (load *ql-libs*))) ; eventual, not yet installed dependencies

(asdf:load-system *asdf-system*)

;;; *** (2) cross-compile ***

(ext:install-c-compiler)

(setf *features* (remove :interpreter *features*)) ; now obsolete, see #-ecl-bytecmp

(load (merge-pathnames (format nil "platforms/~A/cross-compile"
                               #+android "android"
                               #+ios     "ios")))

(setf *load-verbose*    nil
      *compile-verbose* t)

(setf c::*suppress-compiler-warnings* nil
      c::*suppress-compiler-notes*    nil
      c::*compile-in-constants*       t)

;;(setf c::*compile-print* t) ; for debugging compile errors

;;; --- HACK begin ---

(in-package :asdf/lisp-action)

(defmethod asdf:perform ((o asdf:load-op) (c asdf:cl-source-file))
  (if-let (fasl (first (input-files o c)))
    ;; load above compiled *.fasc instead of cross-compiled *.fas
    (load (cl-user::cc (namestring fasl) "c"))))

(in-package :cl-user)

(defvar *build-fasl-orig* (symbol-function 'c:build-fasl))

#+android
(when (eql :fasl *build-type*)
  (setf c::*ld-libs* ""))

(defun c:build-fasl (file &rest args)
  (when (eql :fasl *build-type*)
    (apply *build-fasl-orig* file args))
  file)

;;; --- HACK end ---

(asdf:make-build *asdf-system*
                 :monolithic t
                 :type       (or *build-type* :static-library)
                 :move-here  *library-path*
                 :init-name  *init-name*)
