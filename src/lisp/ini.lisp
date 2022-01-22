;;; doc-string note: documentation is added where a function is defined;
;;; sometimes this is in file 'ecl_ext.cpp'

(si::trap-fpe t nil) ; ignore floating point exceptions (caused on Qt side)

(in-package :qml)

(defvar *break-on-errors* t
  "If T, call (BREAK) on errors inside of LQML functions defined in C++.")

(defun make-qobject (address)
  ;; for internal use
  (ffi:make-pointer address :pointer-void))

(defun qobject-p (x)
  "args: (x)
  Tests if argument is of type QObject."
  (eql 'si:foreign-data (type-of x)))

(defmacro alias (s1 s2)
  `(setf (symbol-function ',s1) (function ,s2)))

(defmacro ! (fun qobject &rest args)
  ;; legacy, should not be needed, use DEFINE-QT-WRAPPERS instead
  ;; usage:
  ;;   (! "myFunction" *cpp* 1 2 3)
  ;;   (! |myFunction| *cpp* 1 2 3)
  `(qfun ,qobject ,(if (stringp fun) fun (symbol-name fun)) ,@args))

(defun %reference-name ()
  (format nil "%~A%" (gensym)))

(defun qexec (&optional ms)
  (%qexec ms))

(defun qsleep (seconds)
  "args: (seconds)
  Similar to SLEEP, but continuing to process Qt events."
  (%qexec (floor (* 1000 seconds)))
  nil)

(defmacro qsingle-shot (milliseconds function)
  ;; check for LAMBDA, #'LAMBDA
  (if (find (first function) '(lambda function))
      ;; hold a reference (will be called later from Qt event loop)
      `(qrun (lambda ()
               (%qsingle-shot ,milliseconds (setf (symbol-function (intern ,(%reference-name))) ; lambda
                                                  ,function))))
      `(qrun (lambda ()
               (%qsingle-shot ,milliseconds ,function)))))                                      ; 'foo

(defmacro qlater (function)
  "args: (function)
  Calls FUNCTION as soon as the Qt event loop is idle."
  `(qsingle-shot 0 ,function))

(defun %ensure-persistent-function (fun)
  (typecase fun
    (symbol   ; 'foo
     fun)
    (function ; lambda
     ;; hold a reference (will be called later from Qt event loop)
     (setf (symbol-function (intern (%reference-name)))
           fun))))

(defun %make-vector ()
  ;; for internal use (called from 'ecl_ext.cpp')
  (make-array 0 :adjustable t :fill-pointer t))

(defun %break (&rest args)
  ;; for internal use (called from 'ecl_ext.cpp')
  (apply 'break args))

(defun ignore-io-streams ()
  "Needed on Windows to prevent crash on print output (for apps without
  a console window)."
  (setf *standard-output* (make-broadcast-stream)
        *trace-output*    *standard-output*
        *error-output*    *standard-output*
        *terminal-io*     (make-two-way-stream (make-string-input-stream "")
                                               *standard-output*)))

(defmacro tr (source &optional context (plural-number -1))
  "args: (source &optional context plural-number)
  Macro expanding to QTRANSLATE, which calls QCoreApplication::translate().
  Both SOURCE and CONTEXT can be Lisp forms evaluating to constant strings
  (at compile time). The CONTEXT argument defaults to the Lisp file name.
  For the PLURAL-NUMBER, see Qt Assistant."
  ;; see compiler-macro in "tr.lisp"
  (let ((source* (ignore-errors (eval source)))
        (context* (ignore-errors (eval context))))
    `(qml:qtranslate ,(if (stringp context*)
                          context*
                          (if *compile-file-truename* (file-namestring *compile-file-truename*) ""))
                     ,source*
                     ,plural-number)))

(defun %string-or-nil (x)
  (typecase x
    (string
      x)
    (symbol
      (unless (member x '(t nil))
        (symbol-name x)))))

(defun qfind-children (object &optional object-name class-name)
  (%qfind-children object object-name class-name))

(defun qload-c++ (library-name &optional unload)
  (%qload-c++ library-name unload))

(defun define-qt-wrappers (qt-library &rest what)
  "args: (qt-library &rest what)
  Defines Lisp methods for all Qt methods/signals/slots of given library,
  previously loaded with QLOAD-C++.
    (define-qt-wrappers *c++*)          ; generate wrappers
    (define-qt-wrappers *c++* :methods) ; Qt methods only (no slots/signals)
    (my-qt-function *c++* x y)          ; call from Lisp"
  (assert (qobject-p qt-library))
  (let ((all-functions (qapropos* nil qt-library))
        (lispify (not (find :do-not-lispify what))))
    (setf what (remove-if (lambda (x) (find x '(:do-not-lispify t)))
                          what))
    (unless what
      (setf what '(:methods :slots :signals)))
    (dolist (functions (loop :for el :in what :collect
                             (concatenate 'string (string-capitalize el) ":")))
      (dolist (class-functions all-functions)
        (dolist (fun (rest (find functions (cdr class-functions)
                                 :key 'first :test 'string=)))
          (let* ((p (position #\( fun))
                 (qt-name (subseq fun (1+ (position #\Space fun :from-end t :end p)) p))
                 (lisp-name (intern (if lispify
                                        (with-output-to-string (s)
                                          (x:do-string (ch qt-name)
                                            (cond ((upper-case-p ch)
                                                   (format s "-~C" ch))
                                                  ((char= #\_ ch)
                                                   (write-char #\- s))
                                                  (t
                                                   (write-char (char-upcase ch) s)))))
                                        qt-name))))
            ;; there seems to be no simple way to avoid EVAL here
            ;; (excluding non-portable hacks)
            (eval `(defgeneric ,lisp-name (object &rest arguments)))
            (eval `(defmethod ,lisp-name ((object si:foreign-data) &rest arguments)
                     (%qinvoke-method object ,qt-name arguments)))))))))

(defun qinvoke-method (object function-name &rest arguments)
  ;; for internal use
  (%qinvoke-method object function-name arguments))

(defmacro qget (object name)
  `(%qget ,object ,(if (symbolp name)
                       (symbol-name name)
                       name)))

(defmacro qset (object &rest arguments)
  (assert (evenp (length arguments)))
  `(%qset ,object ',(let (name)
                      (mapcar (lambda (x)
                                (setf name (not name))
                                (if (and name (symbolp x))
                                    (symbol-name x)
                                    x))
                              arguments))))

(defun qrun-on-ui-thread (function &optional (blocking t))
  ;; for internal use
  (%qrun-on-ui-thread function blocking))

(defvar *gui-thread* mp:*current-process*)

(defmacro qrun-on-ui-thread* (&body body)
  ;; for internal use
  (let ((values (gensym)))
    `(if (eql *gui-thread* mp:*current-process*)
         ,(if (second body)
              (cons 'progn body)
              (first body))
         (let (,values)
           (qrun (lambda ()
                   (setf ,values (multiple-value-list ,(if (second body)
                                                           (cons 'progn body)
                                                           (first body))))))
           (values-list ,values)))))

(defmacro qrun* (&body body) ; alias
  `(qrun-on-ui-thread* ,@body))

(defun qquit (&optional (exit-status 0) (kill-all-threads t))
  "args: (&optional (exit-status 0) (kill-all-threads t))
  alias: qq
  Terminates LQML. Use this function instead of ECL (ext:quit) to quit
  gracefully. Negative values for EXIT-STATUS will call C abort() instead of
  normal program exit."
  (declare (ignore kill-all-threads)) ; only here to be equivalent to EXT:QUIT 
  (assert (typep exit-status 'fixnum))
  (%qquit exit-status))

(alias qfun qinvoke-method)
(alias qrun qrun-on-ui-thread)
(alias qq   qquit)

;;; for android logging

(defun qlog (arg1 &rest args)
  "args: (arg1 &optional arg2 arg3...)
  For log messages on android.
    (qlog 12)
    (qlog \"width\" 10 \"height\" 20)
    (qlog \"x ~A y ~A\" x y)"
  (%qlog (if (and (stringp arg1)
                  (find #\~ arg1))
             (apply 'format nil arg1 args)
             (x:join (mapcar 'princ-to-string (cons arg1 args))))))

