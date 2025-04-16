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

