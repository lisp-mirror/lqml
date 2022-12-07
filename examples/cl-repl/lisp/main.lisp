(in-package :editor)

#+mobile
(defconstant +version+ 1)

#+mobile ;; check version
(let* ((file (merge-pathnames ".version"))
       (exists (probe-file file))
       (write (not exists)))
  (when (and exists
             (> +version+
                (parse-integer (alexandria:read-file-into-string file))))
    (copy-all-asset-files) ; asset files may have changed
    (setf write t))
  (when write
    (alexandria:write-string-into-file
     (princ-to-string +version+) file :if-exists :supersede)))

(ignore-errors ; don't hang on startup
 (load (merge-pathnames ".eclrc")))

#+mobile
(when qml::*remote-ip*
  (qsingle-shot 1000 'auto-reload-qml))
