(in-package :qml-user)

(defun request ()
  "Runs request in a thread, returns after thread finished."
  (q> |playing| "busy" t) ; start animation
  (let (response)
    ;; worker thread
    (mp:process-run-function
     :request
     (lambda ()
       (sleep 3) ; working hard...
       (setf response :ok)
       (qexit)))
    ;; main thread
    (qexec (* 60 1000)) ; timeout (ms)
    (q> |playing| "busy" nil) ; stop animation
    response))

(qsingle-shot 1000 'request)
