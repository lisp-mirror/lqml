(defpackage :qml-tr
  (:use :cl :qml))

(in-package :qml-tr)

(defparameter *sources* (make-hash-table :test 'equal))

(defvar cl-user::*tr-path* *default-pathname-defaults*)

(let ((tr.h (merge-pathnames "tr.h" cl-user::*tr-path*)))
  (when (probe-file tr.h)
    (delete-file tr.h))
  (format t "~&creating ~S~%" tr.h)
  (define-compiler-macro tr (&whole form src &optional con (n -1))
    (let* ((source (ignore-errors (eval src)))
           (context* (ignore-errors (eval con)))
           (context (if (stringp context*)
                        context*
                        (file-namestring *compile-file-truename*))))
      (with-open-file (out tr.h :direction :output :if-exists :append :if-does-not-exist :create)
        (if (stringp source)
            (unless (gethash (cons source context) *sources*)
              (setf (gethash (cons source context) *sources*) t)
              (format out "QCoreApplication::translate(~S, ~S, 0~A);~%"
                      context
                      source
                      (if (= -1 n) "" (format nil ", ~D" n))))
            (error "[TR:error] ~S from context ~S doesn't evaluate to a string" src context))))
    form))

