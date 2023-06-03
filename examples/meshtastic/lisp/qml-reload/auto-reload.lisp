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
    (flet ((repeat ()
             (qsingle-shot 500 'watch-files)))
      (unless files
        (dolist (file (directory (merge-pathnames "../../qml/**/*.qml" *dir*)))
          (push file files)))
      (let ((curr 0))
        (dolist (file files)
          (let ((date (file-write-date file)))
            (unless date ; might be NIL while saving
              (return-from watch-files (repeat)))
            (incf curr date)))
        (when (/= secs curr)
          (unless (zerop secs)
            (qml:reload))
          (setf secs curr)))
      (repeat))))

(watch-files)

