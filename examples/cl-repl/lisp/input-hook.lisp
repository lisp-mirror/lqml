;;; idea & most code from "ecl-readline.lisp"

(defpackage input-hook
  (:use :cl)
  (:export
   #:add))

(provide :input-hook)

(in-package :input-hook)

(defvar *functions* nil)

(defun add (function)
  (let ((stream (make-instance 'input-hook-stream)))
    (push (cons stream function) *functions*)
    stream))

(defclass input-hook-stream (gray:fundamental-character-input-stream)
  ((buffer :initform (make-string 0))
   (index  :initform 0)))

(defmethod gray:stream-read-char ((stream input-hook-stream))
  (if (ensure-stream-data stream)
      (with-slots (buffer index) stream
        (let ((ch (char buffer index)))
          (incf index)
          ch))
      :eof))

(defmethod gray:stream-unread-char ((stream input-hook-stream) character)
  (with-slots (index) stream
    (when (> index 0)
      (decf index))))

(defmethod gray:stream-listen ((stream input-hook-stream))
  nil)

(defmethod gray:stream-clear-input ((stream input-hook-stream))
  nil)

(defmethod gray:stream-peek-char ((stream input-hook-stream))
  (if (ensure-stream-data stream)
      (with-slots (buffer index) stream
        (char buffer index))
      :eof))

(defun ensure-stream-data (stream)
  (with-slots (buffer index) stream
    (when (= index (length buffer))
      (setf buffer (funcall (cdr (assoc stream *functions*)))
            index 0))
    buffer))
