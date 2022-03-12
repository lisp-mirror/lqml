;;; trivial QML auto reload during development for mobile

(in-package :qml)

#+(or android ios)
(defun remote-ip ()
  (terpri *query-io*)
  (princ "Please enter WiFi IP of desktop computer (hit RET to skip): "
         *query-io*)
  (let ((ip (read-line *query-io*)))
    (unless (x:empty-string ip)
      (format nil "http://~A:8080/" ip))))

#+(or android ios)
(defvar *remote-ip* #+interpreter nil
                    #-interpreter #.(remote-ip))

#+(or android ios)
(defun load* (file)
  (load (make-string-input-stream
         (funcall (%sym 'curl :qml)
                  (x:cc *remote-ip* file)))))

(export 'load*)

#+(or android ios)
(defun qml:view-status-changed (status)
  (when (= 1 status)
    (load* "lisp/qml-reload/on-reloaded.lisp")))

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
                (qset *engine* |baseUrl| *remote-ip*)
                (let ((src (qget *quick-view* |source|)))
                  (qset *quick-view* |source| (subseq src #.(length "qrc:///")))))
              (qml:reload)))
        (setf secs curr)))
    (qsingle-shot 250 'auto-reload-qml)))

#+(or android ios)
(export 'auto-reload-qml)
