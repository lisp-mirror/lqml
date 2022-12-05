(in-package :qml)

(defvar *qml-file* nil)

(defun reload-qml-file ()
  (qlater 'qml-item-from-file))

(defun qml-item-from-file (&optional (file *qml-file*) (name "my"))
  (setf *qml-file* file)
  (let ((item (qjs |createItem| ui:*dynamic* file)))
    (when item
      (qset item |parent| (root-item))
      (qset item |objectName| name)
      item)))

(export (list 'qml-item-from-file 'reload-qml-file))
