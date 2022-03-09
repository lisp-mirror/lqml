(in-package :cl-user)

(defparameter *dir* *load-truename*)

(defvar *template* (with-open-file (s (merge-pathnames ".template.qml" *dir*))
                     (let ((str (make-string (file-length s))))
                       (read-sequence str s)
                       str)))

(defun create-qml-loaders ()
  (dolist (file (directory (merge-pathnames "ext/**/*.qml" *dir*)))
    (let* ((name (namestring file))
           (p (1+ (search "/ext/" name)))
           (loader (concatenate 'string (subseq name 0 p) "." (subseq name p))))
      (unless (probe-file loader)
        (ensure-directories-exist loader)
        (with-open-file (s loader :direction :output)
          (let ((new (subseq name p)))
            (format t "~&creating .~A~%" new)
            (format s *template* (subseq name p))))))))

(create-qml-loaders)
