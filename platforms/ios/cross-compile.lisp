;;; cross-compile for iOS arm64
;;;
;;; expects the below environment variables to be set in a script which
;;; needs to call this file

(defvar *architecture* "aarch64-apple-darwin")

(pushnew :ios     *features*)
(pushnew :aarch64 *features*)

(require :cmp)

(defun env (name)
  (ext:getenv name))

(defvar *ecl-ios* (env "ECL_IOS"))

(defun cc (&rest args)
  (apply 'concatenate 'string args))

(setf compiler::*ecl-include-directory* (cc *ecl-ios* "/include/")
      compiler::*ecl-library-directory* (cc *ecl-ios* "/lib/")
      compiler::*cc*       (env "CC")
      compiler::*cc-flags* (env "CFLAGS")
      compiler::*ld*       "ld"
      compiler::*ld-flags* (env "LDFLAGS"))

