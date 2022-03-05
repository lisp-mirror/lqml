;;; trivial QML auto reload during development (desktop only), see:
;;;
;;;   lqml run.lisp -auto

(in-package :qml-user)

(defvar *dir* *load-truename*)

(defun qml:view-status-changed (status)
  (when (= 1 status)
    (load (merge-pathnames "on-reloaded" *dir*))))

(let ((secs 0)
      files)
  (defun watch-files ()
    (unless files
      (dolist (file (directory (merge-pathnames "../../qml/**/*.qml" *dir*)))
        (push file files)))
    (let ((curr 0))
      (dolist (file files)
        (incf curr (file-write-date file)))
      (when (/= secs curr)
        (unless (zerop secs)
          (qml:reload))
        (setf secs curr)))
    (qsingle-shot 250 'watch-files)))

(watch-files)

