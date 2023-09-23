;;; hack to generate 'help.htm' from both Lisp and Qt/C++ doc strings.
;;; usage: 'lqml .make-doc.lisp'

(in-package :qml-user)

(defparameter *help* nil)
  
(defun add-cpp-docu ()
  (with-open-file (s "../src/cpp/ecl_ext.cpp")
    (let (curr ex)
      (flet ((add-curr ()
               (when curr
                 (push (reverse curr) *help*)
                 (setf curr nil)))
             (trim (str)
               (string-trim '(#\/) str)))
        (x:while-it (read-line s nil nil)
          (let ((line (string-trim " " x:it)))
            (when (x:starts-with "///" line)
              (when (x:starts-with "cl_object " ex)
                (add-curr)
                (let* ((pos (search "///" ex :start2 3)) ; exception: Lisp name at end of line
                       (fun (if pos
                                (trim (subseq ex (+ 3 pos)))
                                (trim (subseq ex 10)))))
                  (push (if pos
                            fun
                            (substitute #\- #\_ (string-trim "2" (subseq fun 0 (position #\( fun)))))
                        curr)))
              (push (x:cc " " (trim line)) curr))
            (setf ex line)))
        (add-curr)))))

(defun add-lisp-docu ()
  (do-external-symbols (sym (find-package :qml))
    (let ((name (symbol-name sym)))
      (when (and (or (char= #\Q (char name 0))
                     (find name '("DEFINE-QT-WRAPPERS" "TR" "FIND-QUICK-ITEM" "RELOAD" "ROOT-ITEM"
                                  "ENSURE-PERMISSION" "VIEW-STATUS-CHANGED" "COPY-ALL-ASSET-FILES"
                                  "WITH-ROOT-ITEM" "HEX")
                                  :test 'string=))
                 (not (find name '("QQ") :test 'string=)))
        (print name)
        (x:when-it (documentation sym 'function)
          (print :doc)
          (let ((fun  (string-downcase (symbol-name sym)))
                (docu (x:split x:it #\Newline)))
            (unless (string= fun (subseq (second docu) 7))
              (push (cons fun docu) *help*))))))))

(defun help ()
  (setf *help* nil)
  (add-cpp-docu)
  (add-lisp-docu)
  (with-open-file (s "help.htm" :direction :output :if-exists :supersede)
    (format s "<!doctype html>~%~
               <html lang=\"en\">~%~
               <head>~%~
               <title>Function List</title>~%~
               <meta charset=\"utf-8\">~%~
               </head>~%~
               <body>~%~
               <h2><pre>LQML</pre></h2>~%~
               <pre>~%~%")
    (flet ((el (tag x)
             (format nil "<~A>~A</~A>" tag x tag))
           (! (x)
             (format s "~A~%" x))
           (tab ()
             (write-string "  " s)))
      (setf *help* (sort *help* #'string< :key (lambda (x) (string-trim " " (first x)))))
      (dolist (curr *help*)
        (! (el "b" (format nil "~A ~A" (string-trim " " (first curr)) (subseq (string-trim " " (second curr)) 6))))
        (let ((n 2))
          (when (x:starts-with "alias:" (string-trim " " (third curr)))
            (incf n)
            (! (el "b" (subseq (string-trim " " (third curr)) 7))))
          (! "")
          (let ((examples (nthcdr n curr))
                nl)
            (when examples
              (dolist (example examples)
                (if (x:starts-with "   " example)
                    (progn
                      (unless nl
                        (setf nl t)
                        (! ""))
                      (! example))
                    (! example))))))
        (! "")
        (! ""))
      (format s "</pre>~%</body>~%</html>~%"))))

(progn
  (help)
  (qq))
