(defpackage :debug-ui
  (:use :cl :qml)
  (:export
   #:*debug-dialog*))

(in-package :debug-ui)

(defvar *error-output-buffer* (make-string-output-stream))
(defvar *terminal-out-buffer* (make-string-output-stream))
(defvar *gui-debug-io*        nil)
(defvar *gui-debug-dialog*    nil)

(defun ini ()
  (setf *gui-debug-dialog* 'dialogs:debug-dialog)
  (ini-streams)
  (setf *debug-io* *gui-debug-io*))

(defun ini-streams ()
  (setf *error-output* (make-broadcast-stream *error-output*
                                              *error-output-buffer*))
  (setf *terminal-io*  (make-two-way-stream (two-way-stream-input-stream *terminal-io*)
                                            (make-broadcast-stream (two-way-stream-output-stream *terminal-io*)
                                                                   *terminal-out-buffer*))
        *gui-debug-io* (make-two-way-stream (input-hook:add 'handle-debug-io)
                                            (two-way-stream-output-stream *terminal-io*))))

(defun clear-buffers ()
  (dolist (s (list *error-output-buffer*
                   *terminal-out-buffer*))
    (get-output-stream-string s)))

(defun find-quit-restart ()
  ;; find best restart for ':q' (default) to exit the debugger
  (let ((restarts (compute-restarts)))
    (if (= 1 (length restarts))
        ":r1"
        (let ((restart-names (mapcar (lambda (r)
                                       (symbol-name (restart-name r)))
                                     restarts)))
          ;; precedence role
          (dolist (name '("RESTART-TOPLEVEL"
                          "ABORT"
                          "RESTART-QT-EVENTS"))
            (x:when-it (position name restart-names :test 'string=)
              (return-from find-quit-restart (format nil ":r~D" x:it)))))))
  ":q")

(defun handle-debug-io ()
  (let ((cmd (funcall *gui-debug-dialog*
                      (list (cons (get-output-stream-string *error-output-buffer*)
                                  "#d00000")
                            (cons (get-output-stream-string *terminal-out-buffer*)
                                  "black")))))
    (when (string-equal ":q" cmd)
      (setf cmd (find-quit-restart)))
    (format nil "~A~%" (if (x:empty-string cmd) ":q" cmd))))

(ini)

