;;; check target

(let ((arg (first (ext:command-args))))
  (mapc (lambda (name feature)
          (when (search name arg)
            (pushnew feature *features*)))
        (list "/ecl-android" "/ecl-ios")
        (list :android :ios)))

#+(or android ios)
(pushnew :mobile *features*)

(when (probe-file "/etc/sailfish-release")
  (pushnew :sfos *features*))

;;; copy Swank and ECL contrib files (mobile only)

(defun cc (&rest args)
  (apply 'concatenate 'string args))

#+mobile
(defvar *assets* #+android "../platforms/android/assets/lib/"
                 #+ios     "../platforms/ios/assets/Library/")

#+mobile
(defun find-swank ()
  (probe-file (cc *assets* "quicklisp/local-projects/slime/swank.lisp")))

#+mobile
(defun shell (command)
  (ext:run-program "sh" (list "-c" command)))

#+mobile
(progn
  (unless (find-swank)
    (let ((to (cc *assets* "quicklisp/local-projects/slime/")))
      (ensure-directories-exist to)
      (shell (cc "cp -r ../../../slime/src/* " to))))
  (unless (probe-file (cc *assets* "encodings/"))
    (let ((lib (cc (ext:getenv #+android "ECL_ANDROID" #+ios "ECL_IOS")
                   "/lib/ecl-*/")))
      (shell (cc "cp " lib "*.doc " *assets*))
      (shell (cc "cp -r " lib "encodings " *assets*))))
  (shell (cc "rm -r " *assets* "static-files/*")) ; may have changed
  (shell (cc "cp -r ../clog-assets/static-files " *assets*))
  (shell (cc "cp -r ../clog-assets/*.asd " *assets*)))

#+ios
(with-open-file (s (merge-pathnames "static-files/js/boot.js" *assets*)
                   :direction :output :if-exists :append)
  (write-line "ios = true;" s))

#+mobile
(unless (find-swank)
  (error "Swank files missing, please see <LQML root>/slime/src/readme-sources.md"))

;;; compile ASDF system

(require :asdf)
(require :cmp)

(push (merge-pathnames "../")
      asdf:*central-registry*)

(setf *default-pathname-defaults*
      (truename (merge-pathnames "../../../"))) ; LQML root

(defvar *current*
        (let ((name (namestring *load-truename*)))
          (subseq name
                  (length (namestring *default-pathname-defaults*))
                  (1+ (position #\/ name :from-end t)))))

(dolist (file (list "package" "x" "ecl-ext" "ini" "qml")) ; load LQML symbols
  (load (merge-pathnames file "src/lisp/")))

(progn
  (defvar cl-user::*tr-path* (truename (cc *current* "i18n/")))
  (load "src/lisp/tr"))

#-mobile
(progn
  (require :ecl-curl)
  (asdf:make-build "app"
                   :monolithic t
                   :type :static-library
                   :move-here (cc *current* "build/tmp/")
                   :init-name "ini_app"))

#+mobile
(progn
  (pushnew :interpreter *features*)
  (defvar *asdf-system*  "app")
  (defvar *ql-libs*      (cc *current* "ql-libs.lisp"))
  (defvar *init-name*    "ini_app")
  (defvar *library-path* (format nil "~Abuild-~A/tmp/"
                                 *current*
                                 #+android "android"
                                 #+ios     "ios"))
  (defvar *require*      (list :ecl-curl))
  (load "platforms/shared/make"))

;;; rename lib

(let* ((from #-mobile (cc *current* (format nil "build/tmp/app--all-systems.~A"
                                            #+msvc "lib"
                                            #-msvc "a"))
             #+mobile (cc *library-path* "app--all-systems.a"))
       (to   #-msvc "libapp.a"
             #+msvc "app.lib")
       (to*  #-mobile (cc *current* "build/tmp/" to)
             #+mobile (cc *library-path* to)))
  (when (probe-file to*)
    (delete-file to*))
  (rename-file from to))
