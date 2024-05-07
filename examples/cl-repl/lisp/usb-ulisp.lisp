;;; USB connected uLisp (e.g. Arduino)

(in-package :editor)

(defvar *ulisp-mode* nil)

(defparameter *newline* #.(format nil "~C~C" #\Return #\Newline))

(defun ini-usb ()
  (when *ulisp-mode*
    (qt:connect-usb qt:*cpp*)))

(defun send-to-ulisp (form)
  (qt:send-to-ulisp qt:*cpp* (x:cc form *newline*)))

(let (prompt)
  (defun received-from-ulisp (data)
    (let* ((lines (delete-if 'x:empty-string (x:split data *newline*)))
           (len (length lines)))
      (cond ((= len 2)
             (print-eval-output :expression (first lines))
             (setf prompt (second lines)))
            ((> len 2)
             (let ((exp (cons prompt (butlast lines 2)))
                   (value (nth (- len 2) lines)))
               (print-eval-output :expression  (x:join exp #\Newline))
               (print-eval-output (if (x:starts-with "Error:" value)
                                      :error :values)
                                  value))
             (setf prompt (first (last lines))))))
    (values)))

(qlater 'ini-usb)
