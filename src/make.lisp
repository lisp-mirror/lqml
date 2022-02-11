(when (search "/ecl-android/" (first (ext:command-args)))
  (pushnew :android *features*))

(require :asdf)

(push (merge-pathnames "../")
      asdf:*central-registry*)

(setf *default-pathname-defaults*
      (merge-pathnames "../../")) ; LQML root

#-android
(progn
  (asdf:make-build "lqml"
                   :monolithic t
                   :type :static-library
                   :move-here (format nil "platforms/~A/lib/"
                                      #+linux  "linux"
                                      #+darwin "macos")
                   :init-name "ini_LQML")
  (let* ((from (format nil "platforms/~A/lib/lqml--all-systems.a"
                       #+linux  "linux"
                       #+darwin "macos"))
         (to   "liblisp.a")
         (to*  (format nil "platforms/~A/lib/~A"
                       #+linux  "linux"
                       #+darwin "macos"
                       to)))
    (when (probe-file to*)
      (delete-file to*))
    (rename-file from to)))

#+android
(progn
  (defvar *asdf-system*  :lqml)
  (defvar *library-name* "platforms/android/lib/lisp")
  (defvar *init-name*    "ini_LQML")
  (load "platforms/shared/make"))

(terpri)

