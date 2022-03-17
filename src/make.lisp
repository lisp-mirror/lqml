;;; check target

(let ((arg (first (ext:command-args))))
  (mapc (lambda (name feature)
          (when (search name arg)
            (pushnew feature *features*)))
        (list "/ecl-android/" "/ecl-ios/")
        (list :android :ios)))

;;; compile ASDF system

(require :asdf)

(push (merge-pathnames "../")
      asdf:*central-registry*)

(setf *default-pathname-defaults*
      (merge-pathnames "../../")) ; LQML root

#-(or android ios)
(asdf:make-build "lqml"
                 :monolithic t
                 :type :static-library
                 :move-here (format nil "platforms/~A/lib/"
                                    #+linux  "linux"
                                    #+darwin "macos")
                 :init-name "ini_LQML")

#+(or android ios)
(progn
  (pushnew :interpreter  *features*)
  (defvar *asdf-system*  "lqml")
  (defvar *init-name*    "ini_LQML")
  (defvar *library-path* (format nil "platforms/~A/lib/"
                                     #+android "android"
                                     #+ios     "ios"))
  (load "platforms/shared/make"))

;;; rename lib

(let* ((from (format nil "platforms/~A/lib/lqml--all-systems.a"
                     #+(and linux (not android)) "linux"
                     #+darwin  "macos"
                     #+android "android"
                     #+ios     "ios"))
       (to   "liblisp.a")
       (to*  (format nil "platforms/~A/lib/~A"
                     #+(and linux (not android)) "linux"
                     #+darwin  "macos"
                     #+android "android"
                     #+ios     "ios"
                     to)))
  (when (probe-file to*)
    (delete-file to*))
  (rename-file from to))

(terpri)
