;;; example of changing code while it's running

(defun run ()
  (loop
    (ed:pr (random-number) :color "purple" :bold t)
    (sleep 1)))

(defun random-number ()
  (let ((max 3))
    (format nil "random ~D: ~D" max (1+ (random max)))))

;; run above code in separate thread

(defvar *thread* nil)

(defun start ()
  (setf *thread* (mp:process-run-function :random 'run)))

(defun stop ()
  (mp:process-kill *thread*))

;; now you can change the code while it's running:
;;
;; * call 'start'
;; * change value 'max' in 'random-number',
;;   select the function and eval it
;; * to end it, call 'stop'
