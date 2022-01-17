#+ecl
(si::trap-fpe t nil) ; ignore floating point exceptions (they happen on Qt side)

(in-package :qml)

(defvar *break-on-errors* t)

(defun make-qobject (address)
  (ffi:make-pointer address :pointer-void))

(defun qobject-p (x)
  (eql 'si:foreign-data (type-of x)))

(defmacro alias (s1 s2)
  `(setf (symbol-function ',s1) (function ,s2)))

(defmacro ! (fun &rest args)
  `(qfun ,(cadar args) ,fun ,@(rest args)))

(defun %reference-name ()
  (format nil "%~A%" (gensym)))

(defun qexec (&optional ms)
  (%qexec ms))

(defun qsleep (seconds)
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
               (%qsingle-shot ,milliseconds ,function)))))                                        ; 'foo

(defmacro qlater (function)
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
  (make-array 0 :adjustable t :fill-pointer t))

(defun %break (&rest args)
  (apply 'break args))

(defun ignore-io-streams ()
  (setf *standard-output* (make-broadcast-stream)
        *trace-output*    *standard-output*
        *error-output*    *standard-output*
        *terminal-io*     (make-two-way-stream (make-string-input-stream "")
                                               *standard-output*)))

(defmacro tr (source &optional context (plural-number -1))
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
  ;; N.B. This works only for Qt6 functions with the following signature:
  ;;      "QVariant foo(QVariant, ...)" ; max 10 QVariant arguments
  (let ((all-functions (qapropos* nil (ensure-qt-object qt-library)))
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
            (eval `(defmethod ,lisp-name ((object qt-object) &rest arguments)
                     (%qinvoke-method object ,qt-name arguments)))))))))

(defun qinvoke-method (object function-name &rest arguments)
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
  (%qrun-on-ui-thread function blocking))

(defvar *gui-thread* mp:*current-process*)

(defmacro qrun-on-ui-thread* (&body body)
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
  (declare (ignore kill-all-threads)) ; only here to be equivalent to EXT:QUIT 
  (assert (typep exit-status 'fixnum))
  (%qquit exit-status))

(alias qfun qinvoke-method)
(alias qrun qrun-on-ui-thread)
(alias qq   qquit)

;;; for android logging

(defun qlog (arg1 &rest args)
  ;; (qlog 12)
  ;; (qlog 1 "plus" 2 "gives" 3)
  ;; (qlog "x ~A y ~A" x y)
  (%qlog (if (and (stringp arg1)
                  (find #\~ arg1))
             (apply 'format nil arg1 args)
             (x:join (mapcar 'princ-to-string (cons arg1 args))))))

