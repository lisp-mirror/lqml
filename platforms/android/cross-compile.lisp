;;; please use NDK versions >= 19 (with prebuilt standalone toolchain)

(in-package :cl-user)

(pushnew :android *features*)
(pushnew :aarch64 *features*)

(require :cmp)

(defvar *ndk-toolchain* (ext:getenv "ANDROID_NDK_TOOLCHAIN"))
(defvar *ecl-android*   (ext:getenv "ECL_ANDROID"))
(defvar *architecture*  "aarch64-linux-android")

(defun cc (&rest arguments)
  (apply 'concatenate 'string arguments))

(setf c::*ecl-include-directory* (cc *ecl-android* "/include/")
      c::*ecl-library-directory* (cc *ecl-android* "/lib/"))

(defun ecl-config (flags)
  (read-line (ext:run-program (cc *ecl-android* "/bin/ecl-config")
                              (list flags))))

(setf c::*cc*              (let ((path (or (probe-file (cc *ndk-toolchain* "/bin/aarch64-linux-android21-clang"))
                                           (error "clang compiler not found"))))
                             (namestring path))
      c::*ld*              (cc *ndk-toolchain* "/bin/aarch64-linux-android-ld")
      c::*ar*              (cc *ndk-toolchain* "/bin/aarch64-linux-android-ar")
      c::*ranlib*          (cc *ndk-toolchain* "/bin/aarch64-linux-android-ranlib")
      c::*cc-flags*        (cc (ecl-config "--cflags")
                                 " -DANDROID -DPLATFORM_ANDROID -O2 -fPIC -fno-common -D_THREAD_SAFE -I"
                                 *ecl-android* "/build/gmp")
      c::*ld-flags*        (cc "-L" *ecl-android* "/lib -lecl -ldl -lm "
                               "-L" *ndk-toolchain* "/sysroot/usr/lib/aarch64-linux-android/")
      c::*ld-rpath*        nil
      c::*ld-shared-flags* (cc "-shared " c::*ld-flags*)
      c::*ld-bundle-flags* c::*ld-shared-flags*)
      
(format t "~%*** cross compiling for 'aarch64' ***~%")
