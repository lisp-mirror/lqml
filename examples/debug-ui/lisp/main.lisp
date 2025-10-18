(in-package :app)

;; intentional division by zero after 5 seconds
(qsingle-shot 5000 (lambda () (dotimes (i 1) (/ 1 i))))
