(provide :top-level)

;;; The following are modified/simplified functions taken from "src/lsp/top.lsp" (ECL 11.1.1)

(in-package :si)

(defvar *tpl-print-current-hook* nil)

(defparameter *read-string*        nil)
(defparameter *latest-form*        nil)
(defparameter *latest-values*      nil)
(defparameter *uncaught-exception* nil)

(defun tpl-print-current ()
  (let ((name (ihs-fname *ihs-current*)))
    (format t "~&Broken at ~:@(~S~)." name)
    (when (eq name 'si::bytecodes)
      (format t " [Evaluation of: ~S]"
              (function-lambda-expression (ihs-fun *ihs-current*)))))
  #-threads (terpri)
  #+threads (format t " In: ~A.~%" mp:*current-process*)
  (let ((fun (ihs-fun *ihs-current*)))
    (when (and (symbolp fun) (fboundp fun))
      (setf fun (fdefinition fun)))
    (multiple-value-bind (file position)
        (ext:compiled-function-file fun)
      (when file
        (format t " File: ~S (Position #~D)~%" file position)
        (when *tpl-print-current-hook*
          (funcall *tpl-print-current-hook* file position)))))
  (values))

(defun %top-level ()
  (catch *quit-tag*
    (let ((*debugger-hook* nil)
          -)
      (unless *lisp-initialized*
        (setq *lisp-initialized* t)
        (in-package :qml-user))
      (let ((*tpl-level* -1))
        (%tpl))
      0)))

(defun %tpl (&key ((:commands *tpl-commands*) tpl-commands)
             ((:prompt-hook *tpl-prompt-hook*) *tpl-prompt-hook*)
             (broken-at nil)
             (quiet t))
  #-ecl-min
  (declare (c::policy-debug-ihs-frame))
  (let* ((*ihs-base* *ihs-top*)
         (*ihs-top* (if broken-at (ihs-search t broken-at) (ihs-top)))
         (*ihs-current* (if broken-at (ihs-prev *ihs-top*) *ihs-top*))
         (*frs-base* (or (sch-frs-base *frs-top* *ihs-base*) (1+ (frs-top))))
         (*frs-top* (frs-top))
         (*quit-tags* (cons *quit-tag* *quit-tags*))
         (*quit-tag* *quit-tags*)       ; any unique new value
         (*tpl-level* (1+ *tpl-level*))
         (break-level *break-level*)
         values)
    (set-break-env)
    (set-current-ihs)
    (flet ((rep ()
             ;; We let warnings pass by this way "warn" does the
             ;; work.  It is conventional not to trap anything
             ;; that is not a SERIOUS-CONDITION. Otherwise we
             ;; would be interferring the behavior of code that relies
             ;; on conditions for communication (for instance our compiler)
             ;; and which expect nothing to happen by default.
             (handler-bind 
                 ((serious-condition
                   (lambda (condition)
                     (cond ((< break-level 1)
                            ;; Toplevel should enter the debugger on any condition.
                            )
                           (*allow-recursive-debug*
                            ;; We are told to let the debugger handle this.
                            )
                           (t
                            (format t "~&Debugger received error: ~A~%~
                                         Error flushed.~%" condition)
                            (clear-input)
                            (return-from rep t) ;; go back into the debugger loop.
                            )
                           )
                     )))

               (with-grabbed-console
                   (unless quiet
                     (break-where)
                     (setf quiet t))
                 (setq - (locally (declare (notinline %tpl-read))
                           (tpl-prompt)
                           (%tpl-read))
                       values (multiple-value-list
                                  (eval-with-env - *break-env*)))
                 (setf *latest-form*        -
                       *latest-values*      values
                       *uncaught-exception* nil)))))
      (loop
        (when (eql :eof *read-string*)
          (finish-output)
          (return))
        (when
            (catch *quit-tag*
              (if (zerop break-level)
                  (with-simple-restart 
                      (restart-toplevel "Go back to Top-Level REPL.")
                    (rep))
                  (with-simple-restart
                      (restart-debugger "Go back to debugger level ~D." break-level)
                    (rep)))
              nil)
          (setf quiet nil))))))

(defun %tpl-read (&aux (*read-suppress* nil))
  (qml:qprocess-events)
  (finish-output)
  (when *read-string*
    (prog1
       (handler-case (let ((exp (read-from-string *read-string*)))
                       (setf +++ ++ ++ + + *latest-form*
                             *** ** ** * * (first *latest-values*)
                             /// // // / / *latest-values*)
                       exp)
         (error (condition)
           (format nil "[EQL: read error] ~A" condition)))
      (setf *read-string* :eof))))
