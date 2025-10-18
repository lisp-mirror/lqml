(in-package :app)

(defun crash? ()
  (funcall 'no-such-function))

(qsingle-shot 10000 'crash?)
