;; check target
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

(defun cc (&rest args)
  (apply 'concatenate 'string args))

#+mobile
(defvar *assets* #+android "../platforms/android/assets/lib/"
                 #+ios     "../platforms/ios/assets/Library/")

#+mobile
(defun shell (command)
  (ext:run-program "sh" (list "-c" command)))

#+mobile
(let ((blob #+android *assets*
            #+ios (cc *assets* "../Documents/")))
  (ensure-directories-exist blob)
  (shell (cc "cp -r ../blob " blob)))

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

(defun cc (&rest args)
  (apply 'concatenate 'string args))

(progn
  (defvar cl-user::*tr-path* (truename (cc *current* "i18n/")))
  (load "src/lisp/tr"))

#-mobile
(asdf:make-build "app"
                 :monolithic t
                 :type :static-library
                 :move-here (cc *current* "build/tmp/")
                 :init-name "ini_app")

#+mobile
(progn
  (defvar *asdf-system*  "app")
  (defvar *ql-libs*      (cc *current* "ql-libs.lisp"))
  (defvar *init-name*    "ini_app")
  (defvar *library-path* (format nil "~Abuild-~A/tmp/"
                                 *current*
                                 #+android "android"
                                 #+ios     "ios"))
  (defvar *require*      (list :ecl-curl))
  (load "platforms/shared/make"))

;; rename lib
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
