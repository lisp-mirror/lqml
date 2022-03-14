;;; check target

(let ((arg (first (ext:command-args))))
  (mapc (lambda (name feature)
          (when (search name arg)
            (pushnew feature *features*)))
        (list "/ecl-android/" "/ecl-ios/")
        (list :android :ios)))

(require :asdf)

(push (merge-pathnames "../")
      asdf:*central-registry*)

(setf *default-pathname-defaults*
      (truename (merge-pathnames "../../../"))) ; LQML root

(defvar *current*
        (let ((name (namestring *load-truename*)))
          (subseq name
                  (length (namestring *default-pathname-defaults*))
                  (1+ (position #\/ name :from-end t)))))

;; load all LQML symbols
(dolist (file (list "package" "x" "ecl-ext" "ini" "qml"))
  (load (merge-pathnames file "src/lisp/")))

(defun cc (&rest args)
  (apply 'concatenate 'string args))

#-(or android ios)
(asdf:make-build "app"
                 :monolithic t
                 :type :static-library
                 :move-here (cc *current* "build/tmp/")
                 :init-name "ini_app")

#+(or android ios)
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

;; rename lib
(let* ((from #-(or android ios) (cc *current* "build/tmp/app--all-systems.a")
             #+(or android ios) (cc *library-path* "app--all-systems.a"))
       (to   "libapp.a")
       (to*  #-(or android ios) (cc *current* "build/tmp/" to)
             #+(or android ios) (cc *library-path* to)))
  (when (probe-file to*)
    (delete-file to*))
  (rename-file from to))
