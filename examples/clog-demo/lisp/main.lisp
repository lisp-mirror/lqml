(in-package :app)

(defun run-demo ()
  (clog-demo-1:start-demo)
  (q> |visible| ui:*busy* nil))

(qlater 'run-demo)
