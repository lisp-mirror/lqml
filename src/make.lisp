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
      (merge-pathnames "../../")) ; LQML root

#-(or android ios)
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

#+(or android ios)
(progn
  (defvar *asdf-system*  "lqml")
  (defvar *init-name*    "ini_LQML")
  (defvar *library-name* (format nil "platforms/~A/lib/lisp"
                                     #+android "android"
                                     #+ios     "ios"))
  (load "platforms/shared/make"))

(terpri)

