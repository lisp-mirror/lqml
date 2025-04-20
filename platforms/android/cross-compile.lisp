;;; for Qt5.15 & NDK 21 (should also work for Qt6 & NDK 25)

(in-package :cl-user)

(defvar *32bit* nil) ; see DEFVAR in '../shared/make.lisp'

(pushnew :android *features*)

(if *32bit*
    (setf *features* (remove :aarch64 *features*))
    (pushnew :aarch64 *features*))

(require :cmp)

(defun cc (&rest arguments)
  (apply 'concatenate 'string arguments))

(defparameter *ndk-toolchain* (ext:getenv "ANDROID_NDK_TOOLCHAIN"))
(defparameter *ecl-android*   (ext:getenv (cc "ECL_ANDROID" (if *32bit* "_32" ""))))
(defparameter *arch-triple*   (if *32bit* "arm-linux-androideabi" "aarch64-linux-android"))

(setf c::*ecl-include-directory* (cc *ecl-android* "/include/")
      c::*ecl-library-directory* (cc *ecl-android* "/lib/"))

(defun ecl-config (flags)
  (read-line (ext:run-program (cc *ecl-android* "/bin/ecl-config")
                              (list flags))))

(setf c::*cc*              (let ((path (or (probe-file (cc *ndk-toolchain*
                                                           "/bin/"
                                                           (if *32bit* "armv7a-linux-androideabi" *arch-triple*)
                                                           "21-clang")) ; 21: min SDK version
                                           (error "clang compiler not found"))))
                             (namestring path))
      c::*ld*              (cc *ndk-toolchain* "/bin/" *arch-triple* "-ld")
      c::*ar*              (cc *ndk-toolchain* "/bin/llvm-ar")
      c::*ranlib*          (cc *ndk-toolchain* "/bin/llvm-ranlib")
      c::*cc-flags*        (cc (ecl-config "--cflags")
                               " -DANDROID -DPLATFORM_ANDROID -O2 -fPIC -fno-common -D_THREAD_SAFE -I"
                               *ecl-android* "/build/gmp")
      c::*ld-flags*        (cc "-L" *ecl-android* "/lib -lecl -ldl -lm "
                               "-L" *ndk-toolchain* "/sysroot/usr/lib/" *arch-triple* "/")
      c::*ld-rpath*        nil
      c::*ld-shared-flags* (cc "-shared " c::*ld-flags*)
      c::*ld-bundle-flags* c::*ld-shared-flags*)
      
(format t "~%*** cross compiling for '~A' ***~%" (if *32bit* "armv7a" "aarch64"))
