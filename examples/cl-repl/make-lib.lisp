;;; check target

(defvar *32bit* (<= most-positive-fixnum (expt 2 32)))

(let ((arg (first (ext:command-args))))
  (mapc (lambda (name feature)
          (when (search name arg)
            (pushnew feature *features*)))
        (list "/ecl-android" "/ecl-ios")
        (list :android :ios)))

#+(or android ios)
(pushnew :mobile *features*)

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

(defun cc (&rest args)
  (apply 'concatenate 'string args))

(progn
  (defvar cl-user::*tr-path* (truename (cc *current* "i18n/")))
  (load "src/lisp/tr"))

#-mobile
(progn
  (require :ecl-curl)
  (asdf:make-build "lib"
                   :monolithic t
                   :type :fasl
                   :move-here (cc *current* "build/tmp/")))

#+mobile
(progn
  (pushnew :interpreter *features*)
  (defvar *asdf-system*  "lib")
  (defvar *ql-libs*      (cc *current* "ql-libs.lisp"))
  (defvar *build-type*   :fasl)
  (defvar *library-path* (format nil "~Abuild-~A/tmp/"
                                 *current*
                                 #+android "android"
                                 #+ios     "ios"))
  (defvar *require*      (list :ecl-curl))
  (load "platforms/shared/make"))

;;; rename lib

(let* ((from #-mobile (cc *current* "build/tmp/lib--all-systems.fasb")
             #+mobile (cc *library-path* "lib--all-systems.fasb"))
       (to   "lib.fas")
       (to*  #-mobile (cc *current* "build/tmp/" to)
             #+mobile (cc *library-path* to)))
  (when (probe-file to*)
    (delete-file to*))
  (rename-file from to))
