;;; generate indented QML file
;;;
;;; note: giving an 'id' will automatically add an 'objectName' of the same name

(in-package :pal)

(defmacro with-qml-file ((file) &body body)
  (let ((text (gensym)))
    `(let ((,text (with-output-to-string (s)
                    ,@(mapcar (lambda (x) (if (stringp x) `(write-line ,x s) x))
                              body))))
       ;; stream S is intentionally not a gensym
       (with-open-file (s ,file :direction :output :if-exists :supersede)
         (format s "// THIS FILE IS GENERATED~%~%")
         (write-string (%indent-qml ,text) s)
         (qlater (lambda () (format t "~%QML file generated, see ~S~%~%" ,file)))))))

(defmacro qml (first &body body)
  (if (find #\~ first)
      `(progn
         (format s ,first ,@body)
         (terpri s))
      (let ((open-close (and (upper-case-p (char first 0))
                             (not (find #\{ first)))))
        (if body
            `(progn
               ,(if open-close
                    `(write-line ,(format nil "~%~A {" first) s)
                    (if (find #\{ first)
                        `(write-line ,(format nil "~%~A" first))
                        `(write-line ,first s)))
               ,@(mapcar (lambda (x)
                           (if (stringp x)
                               (if (x:starts-with "id:" x)
                                   `(progn
                                      (write-line ,x s)
                                      (write-line ,(format nil "objectName: ~S" (string-trim " " (subseq x 3))) s))
                                   `(write-line ,x s))
                               x))
                         body)
               ,(when open-close `(write-line "}" s)))
            (if (find #\{ first)
                `(write-line ,(format nil "~%~A" first) s)
                `(write-line ,first s))))))

(defun %indent-qml (text)
  (with-output-to-string (out)
    (let ((in (make-string-input-stream text))
          (depth 0))
      (x:while-it (read-line in nil nil)
        (let ((open (count #\{ x:it))
              (close (count #\} x:it)))
          (write-string (make-string (* 2 (- depth (if (= (- open close) -1) 1 0)))) out)
          (write-line (string-left-trim " " x:it) out)
          (setf depth (+ depth open (- close))))))))
