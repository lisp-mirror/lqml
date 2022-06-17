;;; trivial QML auto reload during development for mobile

(in-package :qml)

(defun remote-ip ()
  (terpri *query-io*)
  (princ "Please enter WiFi IP of desktop computer (hit RET to skip): "
         *query-io*)
  (let ((ip (read-line *query-io*)))
    (unless (x:empty-string ip)
      (format nil "http://~A:8080/" ip))))

(defvar *remote-ip* #+interpreter nil
                    #-interpreter #.(remote-ip))

(defun load* (file)
  (load (make-string-input-stream (curl (x:cc *remote-ip* file)))))

(defun qml:view-status-changed (status)
  (when (= 1 status)
    (load* "lisp/qml-reload/on-reloaded.lisp")))

(let ((secs 0)
      (ini t))
  (defun auto-reload-qml ()
    (multiple-value-bind (curr error)
        (ignore-errors
         (parse-integer
          (curl (x:cc *remote-ip* "cgi-bin/qml-last-modified.py"))))
      (when error
        (qlog :auto-reload-qml :error error))
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

(export
 (list 'load*
       'auto-reload-qml))
