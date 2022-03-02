;;; trivial QML auto reload during development (desktop only), see:
;;;
;;;   lqml run.lisp -auto

(in-package :qml-user)

(defun qml:view-status-changed (status)
  (when (= 1 status)
    ;; any ini code goes here
    (app:populate-item-model)))

(let ((secs 0)
      files)
  (defun watch-files ()
    (unless files
      (dolist (file (directory "qml/**/*.qml"))
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
