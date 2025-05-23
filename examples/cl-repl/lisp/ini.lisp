(in-package :qml)

(export
 (list 'my-ip
       #+android '*shell-output*
       #+android 'shell))

;;; convenience functions from :qt

(defun my-ip ()
  ;; qrun*: only when running on main thread can we have return values from Qt
  (qrun* (qt:local-ip qt:*cpp*)))

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

#+(or android ios)
(let ((ecl-rc (merge-pathnames ".eclrc")))
  (unless (probe-file ecl-rc)
    (with-open-file (s ecl-rc :direction :output)
      (format s "(x:when-it (probe-file \"settings/colors.lisp\")~
               ~%  (load x:it))"))))

(ignore-errors ; don't hang on startup
 (load (merge-pathnames ".eclrc")))

;;; check version

#+(or android ios)
(defconstant +version+ 7)

#+(or android ios)
(let ((.version (merge-pathnames ".version")))
  (when (or (not (probe-file .version))
            (> +version+
               (parse-integer (alexandria:read-file-into-string .version))))
    ;; asset files may have changed
    (copy-all-asset-files :keep (list "settings/colors.lisp"))
    (alexandria:write-string-into-file
     (princ-to-string +version+) .version :if-exists :supersede)))

;;; hacks

;; needed for :sockets (linked as static lib)
(asdf:defsystem :sb-bsd-sockets)

;; eventual ssl libs need to be loaded manually
;; TODO: still needed?
#+android
(when (probe-file "libssl.so")
  (ffi:load-foreign-library "libcrypto.so")
  (ffi:load-foreign-library "libssl.so"))

