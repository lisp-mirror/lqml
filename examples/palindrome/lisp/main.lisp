(in-package :pal)

(defun run-animation (&optional first)
  (dolist (move-to (nthcdr (if first 1 0) *move-to-positions*))
    (let ((target 0))
      (dolist (xy move-to)
        (incf target)
        (let ((img (find-quick-item (format nil "img~D" target))))
          (q> |x| img (* 31 (first xy)))
          (q> |y| img (* 31 (second xy)))
          (qsleep 0.05))))            ; delay between item start      (sec)
    (qsleep 4))                       ; duration of animation + pause (sec)
  (qsingle-shot 1500 'run-animation)) ; pause                         (msec)

(qlater (lambda () (run-animation t)))

