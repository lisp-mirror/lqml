(when (search "/ecl-android/" (first (ext:command-args)))
  (pushnew :android *features*))

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

(defun cc (&rest args)
  (apply 'concatenate 'string args))

#-android
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

#+android
(progn
  (defvar *asdf-system*   "app")
  (defvar *ql-libs*       (cc *current* "/ql-libs.lisp"))
  (defvar *library-name*  (cc *current* "/build-android/tmp/app"))
  (defvar *init-name*     "ini_app")
  (defvar *epilogue-code* nil)
  (load "platforms/shared/make"))

