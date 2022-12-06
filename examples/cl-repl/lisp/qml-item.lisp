(in-package :qml)

(defvar *qml-file* nil)

(defun reload-qml-file ()
  (qlater 'qml-item-from-file))

(defun qml-item-from-file (&optional (file *qml-file*) (name "my"))
  (setf *qml-file* file)
  (x:when-it (qjs |createItem| ui:*dynamic* file)
    (qset x:it |parent| (root-item))
    (qset x:it |objectName| name)
    x:it))

(export (list 'qml-item-from-file 'reload-qml-file))
