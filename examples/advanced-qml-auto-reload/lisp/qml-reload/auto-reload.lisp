;;; trivial QML auto reload during development (desktop only), see:
;;;
;;;   lqml run.lisp -auto

(in-package :qml-user)

(defvar *reload-all*  nil)
(defvar *edited-file* nil)

(defparameter *dir* *load-truename*)

(defun reload-main-p ()
  (or *reload-all*
      (string= "main.qml" *edited-file*)))

(defun qml:view-status-changed (status)
  (when (and (= 1 status)
             (reload-main-p))
    (load (merge-pathnames "on-reloaded" *dir*))))

(let ((secs 0))
  (defun watch-files ()
    (let ((sum 0)
          (max 0)
          files)
      ;; don't cache, files might be added/removed during development
      (dolist (file (directory (merge-pathnames "../../qml/**/*.qml" *dir*)))
        (unless (search "/." (namestring file))
          (push file files)))
      (dolist (file files)
        (let ((date (file-write-date file)))
          (when (> date max)
            (setf max date
                  *edited-file* file))
          (incf sum date)))
      (let ((edited (namestring *edited-file*)))
        (setf *edited-file*
              (subseq edited (+ 5 (search "/qml/" edited)))))
      (when (/= secs sum)
        (unless (zerop secs)
          (if (reload-main-p)
              (qml:reload)
              (qjs |reload| *edited-file*)))
        (setf secs sum)))
    (qsingle-shot 250 'watch-files)))

(progn
  (load "qml/.create-qml-loaders")
  (watch-files))
