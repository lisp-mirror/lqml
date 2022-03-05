;;; trivial QML auto reload during development for mobile

(in-package :qml)

#+(or android ios)
(defvar *remote-ip* #-interpreter
                    (format nil "http://~A:8080/"
                            #.(progn
                                (terpri *query-io*)
                                (princ "Please enter WiFi IP of desktop computer: " *query-io*)
                                (read-line *query-io*))))

#+(or android ios)
(defun qml:view-status-changed (status)
  (when (= 1 status)
    (load (make-string-input-stream
           (funcall (%sym 'curl :qml)
                    (x:cc *remote-ip* "lisp/qml-reload/on-reloaded.lisp"))))))

#+(or android ios)
(let ((load t)
      (secs 0)
      (ini t))
  (defun auto-reload-qml ()
    (when load
      (setf load nil)
      (require :ecl-curl)
      (load "curl"))
    (let ((curr (ignore-errors
                 (parse-integer
                  (funcall (%sym 'curl :qml)
                           (x:cc *remote-ip* "cgi-bin/qml-last-modified.py"))))))
      (when (and curr (/= secs curr))
        (when (plusp secs)
          (if ini
              (progn
                (setf ini nil)
                (qset *quick-view* |source| (x:cc *remote-ip* "qml/main.qml")))
              (qml:reload)))
        (setf secs curr)))
    (qsingle-shot 250 'auto-reload-qml)))

#+(or android ios)
(export 'auto-reload-qml)
