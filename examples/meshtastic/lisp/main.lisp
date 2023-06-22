(in-package :app)

(defun ini ()
  (qt:ini)
  (load-settings)
  (msg:load-messages)
  (q> |playing| ui:*loading* nil)
  #+android
  (ensure-permissions :bluetooth-scan :bluetooth-connect) ; android >= 12
  (lora:start-device-discovery (getf lora:*settings* :device "")))

;;; settings

(defvar *file* (merge-pathnames "data/settings.exp"))

(defun load-settings ()
  (when (probe-file *file*)
    (with-open-file (s *file*)
      (setf lora:*settings* (read s)))))

(defun save-settings ()
  (with-open-file (s *file* :direction :output :if-exists :supersede)
    (let ((*print-pretty* nil))
      (prin1 lora:*settings* s))))

(qlater 'ini)
