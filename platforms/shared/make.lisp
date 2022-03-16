;;; cross-compile ASDF system, using the byte-codes compiler as an intermediate
;;; step; this requires an ASDF hack, but won't even try to build useless
;;; cross-compiled *.fas files -- we load byte-compiled *.fasc files instead
;;;
;;; this is both faster and avoids a 'ld' error when cross-compiling for iOS
;;; (unknown flags for static build)
;;;
;;; tested with quite a few Quicklisp libs

(in-package :cl-user)

(dolist (lib *require*)
  (require lib))

;; optional, to be set in 'make.lisp' in your app dir
(defvar *ql-libs* nil)

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
      (load quicklisp-init))))

;;; load ASDF system

(load *ql-libs*) ; eventual, not yet installed dependencies

(asdf:load-system *asdf-system*)

;;; *** (2) cross-compile ***

;;; load and prepare cross-compiler

(ext:install-c-compiler)

(setf *features* (remove :interpreter *features*))

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

;;; --- ASDF hack begin ---

(in-package :asdf/lisp-action)

(defmethod asdf:perform ((o asdf:load-op) (c asdf:cl-source-file))
  (if-let (fasl (first (input-files o c)))
    (progn
      ;; load above compiled *.fasc instead of cross-compiled *.fas
      (setf fasl (cl-user::cc (namestring fasl) "c"))
      (load fasl))))

(in-package :uiop/lisp-build)

(defun* (compile-file*) (input-file &rest keys
                                    &key (compile-check *compile-check*) output-file warnings-file
                                    object-file
                                    &allow-other-keys)
    (when (and object-file (equal (compile-file-type) (pathname object-file)))
      (format t "Whoa, some funky ASDF upgrade switched ~S calling convention for ~S and ~S~%"
              'compile-file* output-file object-file)
      (rotatef output-file object-file))
    (let* ((keywords (remove-plist-keys
                      `(:output-file :compile-check :warnings-file :object-file) keys))
           (output-file
             (or output-file
                 (apply 'compile-file-pathname* input-file :output-file output-file keywords)))
           (physical-output-file (physicalize-pathname output-file))
           (object-file
             (unless (use-ecl-byte-compiler-p)
               (or object-file
                   (compile-file-pathname output-file :type :object)))))
      (multiple-value-bind (output-truename warnings-p failure-p)
          (with-enough-pathname (input-file :defaults *base-build-directory*)
            (with-saved-deferred-warnings (warnings-file :source-namestring (namestring input-file))
              (with-muffled-compiler-conditions ()
                ;; cross-compile *.o file without even trying to build useless
                ;; cross-compiled *.fas file, which would give a 'ld' error
                ;; when cross-compiling for iOS (wrong flags)
                (when (or (not (probe-file object-file))
                          (> (file-write-date input-file)
                             (file-write-date object-file)))
                  (apply 'compile-file input-file
                         :output-file (list* object-file :system-p t keywords)))
                  (let ((name (namestring object-file)))
                    (setf name (cl-user::cc (subseq name 0 (1- (length name))) "fas"))
                    ;; create dummy to make ASDF happy
                    (open name :direction :probe :if-does-not-exist :create)
                    (pathname name)))))
        (values output-truename warnings-p failure-p))))

(in-package :cl-user)

;;; --- ASDF hack end ---

(asdf:make-build *asdf-system*
                 :monolithic t
                 :type       :static-library
                 :move-here  *library-path*
                 :init-name  *init-name*)
