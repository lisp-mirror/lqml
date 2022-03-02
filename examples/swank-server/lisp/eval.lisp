(defpackage :eval
  (:use :cl :qml)
  (:export
   #:*eval-thread*
   #:append-output
   #:eval-in-thread))

(in-package :eval)

(defvar *output-buffer* (make-string-output-stream))
(defvar *prompt*        t)
(defvar *eval-thread*   nil)
(defvar *               nil)
(defvar **              nil)
(defvar ***             nil)

(defun ini-streams ()
  (setf *standard-output* (make-broadcast-stream *standard-output*
                                                 *output-buffer*))
  (setf *trace-output* *standard-output*
        *error-output* *standard-output*))

(defun current-package-name ()
  (if (eql (find-package :cl-user) *package*)
      "CL-USER"
      (car (sort (list* (package-name *package*) (package-nicknames *package*))
                 (lambda (x y) (< (length x) (length y)))))))

(let ((n -1))
  (defun eval-in-thread (text &optional (progress t)) ; called from QML
    (let ((str (string-trim " " text)))
      (unless (x:empty-string str)
        (if *prompt*
            (let ((pkg (if (zerop n) "QML-USER" (current-package-name)))
                  (counter (princ-to-string (incf n))))
              (format t "~%~A [~A] ~A~%~A"
                      pkg
                      counter
                      (make-string (- 50 (length counter) (length pkg)) :initial-element #\-)
                      str))
            (format t "~A~%~%~A" #.(make-string 50 :initial-element #\_) str))
        ;; run eval in its own thread, so UI will remain responsive
        (update-output)
        (when progress
          (show-progress-bar))
        (qsingle-shot 50 (lambda ()
                           (setf *eval-thread*
                                 (mp:process-run-function "LQML REPL top-level"
                                                          (lambda () (do-eval str))))))))))

(defvar *color-values*     "#80b0ff")
(defvar *color-read-error* "orange")
(defvar *color-error*      "#ff8080")

#+ios
(defun escape-smart-quotation (string)
  (dotimes (i (length string))
    (case (char-code (char string i))
      ((8216 8217 8218)
       (setf (char string i) #\'))
      ((171 187 8220 8221 8222)
       (setf (char string i) #\"))))
  string)

(defun do-eval (string)
  (let ((str #+ios (escape-smart-quotation string)
             #-ios string)
        (color *color-read-error*))
    (handler-case
        (let ((exp (read-from-string str)))
          (setf color *color-error*)
          (let ((vals (multiple-value-list (eval exp))))
            (setf *** ** ** * * (first vals))
            (update-output)
            (append-output (format nil "~{~S~^~%~}" vals) *color-values* t))
          (q! |clear| ui:*repl-input*)
          (history-add str))
      (condition (c)
        (show-error c color))))
  (qsingle-shot 50 'eval-exited))

(defun eval-exited ()
  (update-output)
  (show-progress-bar nil)
  (q> |contentX| ui:*flick-output* 0))

(defun show-error (error color)
  (let ((e1 (prin1-to-string error))
        (e2 (princ-to-string error)))
    (append-output e1 color)
    (unless (string= e1 e2)
      (append-output e2 color))))

(defun show-progress-bar (&optional (show t))
  (q> |visible| ui:*progress* show))

;;; output

(defun update-output ()
  (let ((chunk (get-output-stream-string *output-buffer*)))
    (unless (x:empty-string chunk)
      (let ((text (x:string-substitute "<br>" (string #\Newline) (qescape chunk))))
        ;; "insert" is cleaner with formatting than "append"
        (q! |insert| ui:*repl-output*
            (q< |length| ui:*repl-output*)
            (format nil "<pre><font face='Hack'>~A</font></pre>"
                    (x:if-it (search "[LQML:err]" text)
                             (let ((error-text (subseq text x:it)))
                               (x:string-substitute (format nil "<font color='~A'>~A</font>"
                                                            *color-error*
                                                            error-text)
                                                    error-text
                                                    text))
                             text))))
      (q> |cursorPosition| ui:*repl-output*
          (q< |length| ui:*repl-output*)))))

(defun append-output (text &optional (color "#c0c0c0") bold)
  (q! |append| ui:*repl-output*
      (format nil "<pre><font face='Hack' color='~A'>~A~A~A</font></pre>"
              color
              (if bold "<b>" "")
              (x:string-substitute "<br>" (string #\Newline) (qescape text))
              (if bold "</b>" ""))))

;;; command history

(defvar *history*       (make-array 0 :adjustable t :fill-pointer t))
(defvar *history-index* nil)
(defvar *history-file*  ".lqml-repl-history")
(defvar *max-history*   100)

(defun read-saved-history ()
  (when (probe-file *history-file*)
    (let ((i -1))
      (labels ((index ()
                 (mod i *max-history*))
               (next-index ()
                 (incf i)
                 (index)))
        (let ((tmp (make-array *max-history*))) ; ring buffer
          (with-open-file (s *history-file*)
            (x:while-it (read-line s nil nil)
              (setf (svref tmp (next-index)) x:it)))
          (let ((max (min (1+ i) *max-history*)))
            (when (< max *max-history*)
              (setf i -1))
            (dotimes (n max)
              (vector-push-extend (svref tmp (next-index))
                                  *history*))
            (setf *history-index* (length *history*)))))))) ; 1 after last

(let (out)
  (defun history-ini ()
    (read-saved-history)
    (setf out (open *history-file* :direction :output
                    :if-exists :append :if-does-not-exist :create)))
  (defun history-add (line)
    (unless out
      (history-ini))
    (let ((len (length *history*)))
      (when (or (zerop len)
                (string/= line (aref *history* (1- len))))
        (vector-push-extend line *history*)
        (write-line line out)
        (finish-output out)))
    (setf *history-index* (length *history*))) ; 1 after last
  (defun history-move (direction)
    (unless out
      (history-ini))
    (when (and *history-index*
               (plusp (length *history*)))
      (setf *history-index* (if (string= "back" direction)
                                (max (1- *history-index*) 0)
                                (min (1+ *history-index*) (1- (length *history*)))))
      (let ((text (aref *history* *history-index*)))
        (q> |text| ui:*repl-input* text)
        (q> |cursorPosition| ui:*repl-input*
            (- (length text) (if (x:ends-with ")" text) 1 0)))))))

(defun qml::help ()
  (format t "~%~
             ~% :s  (start-swank)~
             ~% :q  (quicklisp)")
  (values))

(progn
  (ini-streams)
  (qlater (lambda ()
            (in-package :qml-user)
            (eval-in-thread "(qml::help)" nil))))

