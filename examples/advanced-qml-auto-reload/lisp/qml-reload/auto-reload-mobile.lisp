;;; trivial QML auto reload during development for mobile

(in-package :qml)

(defvar *reload-all*  nil)
(defvar *edited-file* nil)

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

(defun load-on-reloaded ()
  (load* "lisp/qml-reload/on-reloaded.lisp"))

(defun qml:view-status-changed (status)
  (when (and (= 1 status)
             (reload-main-p))
    (load-on-reloaded)))

(defun reload-main-p ()
  (prog1
      (or *reload-all*
          (string= "main.qml" *edited-file*))
    (when (eql :once *reload-all*)
      (setf *reload-all* nil))))

(let ((secs 0)
      (ini t))
  (defun auto-reload-qml ()
    (multiple-value-bind (curr/file error)
        (ignore-errors
         (x:split (curl (x:cc *remote-ip* "cgi-bin/qml-last-modified.py"))
                  #.(coerce (list #\Return #\Newline) 'string)))
      (when error
        (qlog :auto-reload-qml :error error))
      (when (= 2 (length curr/file))
        (destructuring-bind (curr file)
            curr/file
          (setf curr          (parse-integer curr)
                *edited-file* (string-right-trim '(#\Return #\Newline) file))
          (when (/= secs curr)
            (if ini
                (progn
                  (setf ini nil)
                  (qset *engine* |baseUrl| *remote-ip*)
                  (let ((src (qget *quick-view* |source|)))
                    ;; this causes an initial reload of everything
                    (qset *quick-view* |source| (subseq src #.(length "qrc:///")))
                    (setf *reload-all* :once)))
                (if (reload-main-p)
                    (qml:reload)
                    (qjs |reload| *edited-file*)))
            (setf secs curr)))))
    (qsingle-shot 500 'auto-reload-qml)))

(export
 (list 'load*
       'auto-reload-qml))
