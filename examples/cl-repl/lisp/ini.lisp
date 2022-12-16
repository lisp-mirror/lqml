(in-package :qml)

(export
 (list #+android '*shell-output*
       #+android 'shell))

;;; add function 'shell' (android only)

#+android
(defvar *shell-output* nil)

#+android
(defun shell (command)
  "Run shell commands; example:
  (shell \"df -h\")"
  (let ((s (ext:run-program "sh" (list "-c" command))))
    (setf *shell-output*
          (loop :for line = (read-line s nil nil)
                :while line :collect line)))
  (princ (x:join *shell-output* #\Newline))
  (values))

;;; create default '.eclrc'

#+mobile
(let ((ecl-rc (merge-pathnames ".eclrc")))
  (unless (probe-file ecl-rc)
    (with-open-file (s ecl-rc :direction :output)
      (format s "(x:when-it (probe-file \"settings/colors.lisp\")~
               ~%  (load x:it))"))))

(ignore-errors ; don't hang on startup
 (load (merge-pathnames ".eclrc")))

;;; check version

#+mobile
(defconstant +version+ 2)

#+mobile
(let ((.version (merge-pathnames ".version")))
  (when (or (not (probe-file .version))
            (> +version+
               (parse-integer (alexandria:read-file-into-string .version))))
    ;; asset files may have changed
    (copy-all-asset-files :keep (list "settings/colors.lisp"))
    (alexandria:write-string-into-file
     (princ-to-string +version+) .version :if-exists :supersede)))

#+android
(progn
  ;; copied asset files are read-only by default
  (when (probe-file "settings/")
    (shell "chmod 664 settings/*.lisp")))

;;; hacks

;; needed for :sockets (linked as static lib)
(asdf:defsystem :sb-bsd-sockets)

;; eventual ssl libs (not included in LQML) need to be loaded manually
#+android
(when (probe-file "libssl.so")
  (ffi:load-foreign-library "libcrypto.so")
  (ffi:load-foreign-library "libssl.so"))

