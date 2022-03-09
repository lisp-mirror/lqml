;;; trivial QML auto reload during development for mobile

(in-package :qml)

#+(or android ios)
(defvar *reload-all*  nil)
(defvar *edited-file* nil)

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
(defun qml:view-status-changed (status)
  (when (and (= 1 status)
             (reload-main-p))
    (load (make-string-input-stream
           (funcall (%sym 'curl :qml)
                    (x:cc *remote-ip* "lisp/qml-reload/on-reloaded.lisp"))))))

#+(or android ios)
(defun reload-main-p ()
  (or *reload-all*
      (string= "main.qml" *edited-file*)))

#+(or android ios)
(let ((load t)
      (secs 0)
      (ini t))
  (defun auto-reload-qml ()
    (when load
      (setf load nil)
      (require :ecl-curl)
      (load "curl"))
    (let ((curr/file (ignore-errors (x:split (funcall (%sym 'curl :qml)
                                                      (x:cc *remote-ip*
                                                            "cgi-bin/qml-last-modified.py"))
                                             #.(coerce (list #\Return #\Newline) 'string)))))
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
                    (qset *quick-view* |source| (subseq src #.(length "qrc:///")))))
                (if (reload-main-p)
                    (qml:reload)
                    (qjs |reload| *edited-file*)))
            (setf secs curr)))))
    (qsingle-shot 250 'auto-reload-qml)))

#+(or android ios)
(export 'auto-reload-qml)
