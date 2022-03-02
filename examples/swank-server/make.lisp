;;; check target

(let ((arg (first (ext:command-args))))
  (mapc (lambda (name feature)
          (when (search name arg)
            (pushnew feature *features*)))
        (list "/ecl-android/" "/ecl-ios/")
        (list :android :ios)))

;;; copy Swank and ECL contrib files (mobile only)

(defun cc (&rest args)
  (apply 'concatenate 'string args))

#+(or android ios)
(defvar *assets* #+android "../platforms/android/assets/lib/"
                 #+ios     "../platforms/ios/assets/Library/")

#+(or android ios)
(defun find-swank ()
  (probe-file (cc *assets* "quicklisp/local-projects/slime/swank.lisp")))

#+(or android ios)
(defun shell (command)
  (ext:run-program "sh" (list "-c" command)))

#+(or android ios)
(progn
  (unless (find-swank)
    (let ((to (cc *assets* "quicklisp/local-projects/slime/")))
      (ensure-directories-exist to)
      (shell (cc "cp -r ../../../slime/src/* " to))))
  (unless (probe-file (cc *assets* "encodings"))
    #+android
    (let ((lib (cc (ext:getenv "ECL_ANDROID") "/lib/ecl-*/")))
      (shell (cc "cp " lib "*.asd " *assets*))
      (shell (cc "cp " lib "*.fas " *assets*))
      (shell (cc "cp " lib "*.doc " *assets*))
      (shell (cc "cp -r " lib "encodings " *assets*)))
    #+ios
    (let ((lib (cc (ext:getenv "ECL_IOS") "/lib/ecl-*/")))
      (shell (cc "cp " lib "*.doc " *assets*))
      (shell (cc "cp -r " lib "encodings " *assets*)))))

#+(or android ios)
(unless (find-swank)
  (error "Swank files missing, please see <LQML root>/slime/src/readme-sources.md"))

;;; compile ASDF system

(require :asdf)

(push (merge-pathnames "../")
      asdf:*central-registry*)

(setf *default-pathname-defaults*
      (truename (merge-pathnames "../../../"))) ; LQML root

(defvar *current*
        (let ((name (namestring *load-truename*)))
          (subseq name
                  (length (namestring *default-pathname-defaults*))
                  (position #\/ name :from-end t))))

;; load all LQML symbols
(dolist (file (list "package" "x" "ecl-ext" "ini" "qml"))
  (load (merge-pathnames file "src/lisp/")))

#-(or android ios)
(progn
  (asdf:make-build "app"
                   :monolithic t
                   :type :static-library
                   :move-here (cc *current* "/build/tmp/")
                   :init-name "ini_app")
  (let* ((from (cc *current* "/build/tmp/app--all-systems.a"))
         (to   "libapp.a")
         (to*  (cc *current* "/build/tmp/" to)))
    (when (probe-file to*)
      (delete-file to*))
    (rename-file from to)))

#+(or android ios)
(progn
  (pushnew :interpreter *features*)
  (defvar *asdf-system*   "app")
  (defvar *ql-libs*       (cc *current* "/ql-libs.lisp"))
  (defvar *init-name*     "ini_app")
  (defvar *library-name*  (format nil "~A/build-~A/tmp/app"
                                  *current*
                                  #+android "android"
                                  #+ios     "ios"))
  (defvar *epilogue-code* nil)
  (load "platforms/shared/make"))

