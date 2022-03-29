;;; this file will be loaded every time QML has been reloaded

(in-package :qsoko)

(progn
  (let ((*no-delete* t))
    (set-maze))
  (q> |to| ui:*level* (1- (length *my-mazes*))))
