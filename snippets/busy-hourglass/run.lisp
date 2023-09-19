(in-package :qml-user)

(defun load-huge-library ()
  (q> |running| "hourglass" t)         ; start animation
  ;; run task in thread
  (mp:process-run-function
   :loading
   (lambda ()
     (sleep 10) ; loading patiently...
     (q> |running| "hourglass" nil)))) ; stop animation

(qsingle-shot 1000 'load-huge-library)
