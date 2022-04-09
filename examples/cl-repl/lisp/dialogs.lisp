(defpackage :dialogs
  (:use :cl :qml)
  (:export
   #:query-dialog
   #:debug-dialog
   #:get-file-name
   #:exited
   #:push-dialog
   #:pop-dialog
   #:*file-name*))

(in-package :dialogs)

(defvar *file-name* nil)
(defvar *callback*  nil)

(defun push-dialog (name)
  "Pushes dialog NAME onto the StackView."
  #+ios
  (disable-clipboard-menu nil)
  (qjs |pushDialog| ui:*main* (string-downcase name)))

(defun pop-dialog ()
  "Pops the currently shown dialog, returning T if there was a dialog to pop."
  #+ios
  (disable-clipboard-menu)
  (prog1
      (> (q< |depth| ui:*main*) 1)
    (qjs |popDialog| ui:*main*)
    (exited)))

(defun wait-while-transition ()
  ;; needed for evtl. recursive calls
  (x:while (q< |busy| ui:*main*)
    (qsleep 0.1)))

(defun query-dialog (query)
  (unless (x:empty-string query)
    (q> |text| ui:*query-text* (string-trim '(#\Newline) query)))
  (q! |clear| ui:*query-input*)
  (wait-while-transition)
  (push-dialog :query)
  (q! |forceActiveFocus| ui:*query-input*)
  (q! |showKeyboard| ui:*main* t) ; needed on recursive calls
  (wait-for-closed)
  (pop-dialog)
  (qlater (lambda () (editor:ensure-focus :show)))
  (q< |text| ui:*query-input*))

(defun append-debug-output (text color bold)
  (qjs |appendOutput| ui:*debug-model*
       (list :m-text  text
             :m-color color
             :m-bold  bold)))

(defun debug-dialog (messages)
  (q! |clear| ui:*debug-model*)
  (q> |text| ui:*debug-input* ":q")
  (dolist (text/color messages)
    (let* ((text (string-trim '(#\Newline) (car text/color)))
           (color (cdr text/color))
           (bold (not (string= "black" color)))) ; boolean
      (append-debug-output text color bold)))
  (wait-while-transition)
  (push-dialog :debug)
  (q! |forceActiveFocus| ui:*debug-input*)
  (qsingle-shot 500 (lambda () (q! |positionViewAtEnd| ui:*debug-text*)))
  (wait-for-closed)
  (pop-dialog)
  (qlater (lambda () (editor:ensure-focus :show)))
  (q< |text| ui:*debug-input*))

(let ((exited t))
  (defun wait-for-closed ()
    (setf exited nil)
    ;; busy waiting is safer than suspending a thread, especially on mobile
    (x:while (not  exited)
      (qsleep 0.1)))
  (defun exited () ; called from QML
    (setf exited t)))

;; file browser

(let ((1st t))
  (defun get-file-name (&optional callback focus)
    #+android
    (ensure-permissions :write-external-storage)
    (qjs |showKeyboard| ui:*main* nil)
    (when 1st
      (setf 1st nil)
      (set-file-browser-path ":home"))
    (setf *callback* callback)
    ;; force update
    (dolist (folder (list "" (q< |folder| ui:*folder-model*)))
      (q> |folder| ui:*folder-model* folder))
    (q> |editMode| ui:*file-browser* nil)
    (push-dialog :file)
    (when focus
      (qsingle-shot 500 (lambda () (q! |forceActiveFocus| ui:*path*))))))

(defun directory-p (path)
  (not (or (pathname-name path)
           (pathname-type path))))

(defun set-file-name (file-name) ; called from QML
  (let ((name (remove-if (lambda (ch) (find ch "*?\\")) file-name)))
    (if (directory-p name)
        (set-file-browser-path name)
        (progn
          (setf *file-name* name)
          (when *callback*
            (funcall *callback*))
          ;; QLATER: prevent crash (internal threads are a bitch)
          (qlater 'pop-dialog)))))

(defun rename-file* (from to) ; called from QML
  (ignore-errors (rename-file from to)))

(defun location (name)
  (cond ((string= ":data" name)
         #+mobile
         (progn
           #+android "/sdcard/Documents/"
           #+ios     (namestring (truename (merge-pathnames "../Documents/"))))
         #-mobile
         (namestring (merge-pathnames "Documents/" (user-homedir-pathname))))
        ((string= ":home" name)
         (namestring *default-pathname-defaults*))))

(defun set-file-browser-path (path) ; called from QML
  (let ((url (x:cc "file://" (if (x:starts-with ":" path)
                                 (location path)
                                 path))))
    (unless (x:ends-with "/" url)
      (setf path* (x:cc url "/")))
    (q> |folder| ui:*folder-model* url)))
