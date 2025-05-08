;;; check target

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
      (merge-pathnames "../../")) ; LQML root

#-mobile
(asdf:make-build "lqml"
                 :monolithic t
                 :type :static-library
                 :move-here (format nil "platforms/~A/lib/"
                                    #+linux  "linux"
                                    #+darwin "macos"
                                    #+win32  "windows")
                 :init-name "ini_LQML")

#+mobile
(progn
  (defvar *asdf-system*  "lqml")
  (defvar *init-name*    "ini_LQML")
  (defvar *library-path* (format nil "platforms/~A/lib/"
                                     #+android "android"
                                     #+ios     "ios"))
  (load "platforms/shared/make"))

;;; rename lib

(let* ((from (format nil "platforms/~A/lib/lqml--all-systems.~A"
                     #+(and linux  (not android)) "linux"
                     #+(and darwin (not ios))     "macos"
                     #+win32   "windows"
                     #+android "android"
                     #+ios     "ios"
                     #+msvc    "lib"
                     #-msvc    "a"))
       (to   #+msvc "lisp.lib"
             #-msvc (format nil "liblisp~A.a"
                            #+android (if (<= most-positive-fixnum (expt 2 32)) "32" "")
                            #-android ""))
       (to*  (format nil "platforms/~A/lib/~A"
                     #+(and linux  (not android)) "linux"
                     #+(and darwin (not ios))     "macos"
                     #+win32   "windows"
                     #+android "android"
                     #+ios     "ios"
                     to)))
  (when (probe-file to*)
    (delete-file to*))
  (rename-file from to))

(terpri)
