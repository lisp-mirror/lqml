;; hack to load 'main.lisp' into editor and display 'main.qml'

(let ((path (merge-pathnames "examples/QML/Draw/")))
  (defun set-file-name (name)
    (setf dialogs:*file-name*
          (namestring (merge-pathnames name path)))))

(dolist (file (list "main.lisp"
                    "main.qml"))
  (set-file-name file)
  (ed::do-open-file)
  (when (string= "lisp" (pathname-type dialogs:*file-name*))
    (load dialogs:*file-name*)))
