;; hack to load 'main.lisp' into editor and display 'main.qml'

(defparameter *files*
  '("main.lisp"
    "main.qml"))

(let ((dir (merge-pathnames "examples/QML/Draw/")))
  (dolist (file *files*)
    (let ((path (merge-pathnames file dir)))
      (ed::do-open-file path)
      (when (string= "lisp" (pathname-type path))
        (load path)))))
