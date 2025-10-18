(defpackage :dialogs
  (:use :cl :qml)
  (:export
   #:debug-dialog
   #:exited
   #:push-dialog
   #:pop-dialog))

(in-package :dialogs)

(defvar *callback* nil)

(defun push-dialog (name)
  "Pushes dialog NAME onto the StackView."
  (qjs |pushDialog| ui:*main* (string-downcase name)))

(defun pop-dialog ()
  "Pops the currently shown dialog, returning T if there was a dialog to pop."
  (prog1
      (> (q< |depth| ui:*main*) 1)
    (qjs |popDialog| ui:*main*)))

(defun wait-while-transition ()
  ;; needed for evtl. recursive calls
  (x:while (q< |busy| ui:*main*)
    (qsleep 0.1)))

(defun append-debug-output (text color bold)
  (qjs |appendOutput| ui:*d-debug-model*
       (list :text  text
             :color color
             :bold  bold)))

(defun debug-dialog (messages)
  (qrun*
   (q! |clear| ui:*d-debug-model*)
   (q> |text| ui:*d-debug-input* ":q")
   (dolist (text/color messages)
     (let* ((text (string-trim '(#\Newline) (car text/color)))
            (color (cdr text/color))
            (bold (not (string= "black" color)))) ; boolean
       (append-debug-output text color bold)))
   (wait-while-transition)
   (push-dialog :debug)
   (q! |forceActiveFocus| ui:*d-debug-input*)
   (qsingle-shot 500 (lambda () (q! |positionViewAtEnd| ui:*d-debug-text*)))
   (wait-for-closed)
   (q< |text| ui:*d-debug-input*)))

(let (waiting)
  (defun wait-for-closed ()
    (setf waiting t)
    ;; busy waiting is safer than suspending a thread, especially on mobile
    (x:while waiting
      (qsleep 0.1))
    (pop-dialog))
  (defun exited () ; called from QML
    (unless waiting
      (pop-dialog))
    (setf waiting nil)))

