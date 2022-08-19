;;; doc-string note: documentation is added where a function is defined;
;;; sometimes this is in file 'ecl_ext.cpp'

(si::trap-fpe t nil) ; ignore floating point exceptions (caused on Qt side)

(in-package :qml)

(defvar *break-on-errors* t
  "If T, call (BREAK) on errors inside of LQML functions defined in C++.")

(defstruct (qt-object (:constructor qt-object (address)))
  (address 0 :type integer))

(defun %qml-name (name)
  (cond ((string= "QQuickView" name)
         name)
        ((string= "QQuickItem" name)
         "Item")
        (t
         (subseq name
                 (let ((start 0))
                   (dolist (q '("QDeclarativeGeo" "QDeclarative" "QQuick" "QQml" "Qml"))
                     (when (x:starts-with q name)
                       (setf start (length q))
                       (return)))
                   start)
                 (or (search "Item" name)
                     (position #\_ name))))))

(defmethod print-object ((object qt-object) s)
  (print-unreadable-object (object s :type nil :identity nil)
    (multiple-value-bind (class name address)
        (qt-object-info object)
      (format s "~A ~S ~A"
              (%qml-name class)
              name
              (if (zerop address)
                  "NULL"
                  (format nil "0x~X" address))))))

(defun qeql (object-1 object-2)
  "args: (qt-object-1 qt-object-2)
  Returns T if passed QT-OBJECTs are pointer equal."
  (assert (and (qt-object-p object-1)
               (qt-object-p object-2)))
  (= (qt-object-address object-1)
     (qt-object-address object-2)))

(defmacro ! (fun qt-object &rest args)
  ;; legacy, should not be needed, use DEFINE-QT-WRAPPERS instead
  ;; usage:
  ;;   (! "myFunction" *cpp* 1 2 3)
  ;;   (! |myFunction| *cpp* 1 2 3)
  `(qfun ,qt-object ,(if (stringp fun) fun (symbol-name fun)) ,@args))

(defun %reference-name ()
  (format nil "%~A%" (gensym)))

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

(defun qexec (&optional ms)
  (qrun* (%qexec ms)))

(defun qsleep (seconds)
  "args: (seconds)
  Similar to SLEEP, but continuing to process Qt events."
  (qrun* (%qexec (floor (* 1000 seconds))))
  nil)

(defmacro qsingle-shot (milliseconds function)
  ;; check for LAMBDA, #'LAMBDA
  (if (find (first function) '(lambda function))
      ;; hold a reference (will be called later from Qt event loop)
      `(qrun* (%qsingle-shot ,milliseconds (setf (symbol-function (intern ,(%reference-name))) ; lambda
                                                 ,function)))
      `(qrun* (%qsingle-shot ,milliseconds ,function))))                                       ; 'foo

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
                          (file-namestring (or *compile-file-truename*
                                               *load-truename*)))
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
  ;; for internal use
  (%qfind-children object object-name class-name))

(defun qload-c++ (library-name &optional unload)
  (qrun* (%qload-c++ library-name unload)))

(defun define-qt-wrappers (qt-library &rest what)
  "args: (qt-library &rest what)
  Defines Lisp methods for all Qt methods/signals/slots of given library,
  previously loaded with QLOAD-C++.
    (define-qt-wrappers *c++*)          ; generate wrappers
    (define-qt-wrappers *c++* :methods) ; Qt methods only (no slots/signals)
    (my-qt-function *c++* x y)          ; call from Lisp"
  (assert (qt-object-p qt-library))
  (let ((all-functions (qapropos* nil qt-library)))
    (unless what
      (setf what '(:methods :slots :signals)))
    (dolist (functions (loop :for el :in what :collect
                             (concatenate 'string (string-capitalize el) ":")))
      (dolist (class-functions all-functions)
        (dolist (fun (rest (find functions (cdr class-functions)
                                 :key 'first :test 'string=)))
          (let* ((p (position #\( fun))
                 (qt-name (subseq fun (1+ (position #\Space fun :from-end t :end p)) p))
                 (qt-name* (let ((name (copy-seq qt-name)))
                             (when (x:ends-with "2" name)
                               (setf (char name (1- (length name)))
                                     #\*))
                             name))
                 (lisp-name (intern (with-output-to-string (s)
                                      (x:do-string (ch qt-name*)
                                        (cond ((upper-case-p ch)
                                               (format s "-~C" ch))
                                              ((char= #\_ ch)
                                               (write-char #\- s))
                                              (t
                                               (write-char (char-upcase ch) s))))))))
            ;; there seems to be no simple way to avoid EVAL here
            ;; (excluding non-portable hacks)
            (eval `(defgeneric ,lisp-name (object &rest arguments)))
            (eval `(defmethod ,lisp-name ((object qt-object) &rest arguments)
                     (%qinvoke-method object ,qt-name arguments)))))))))

(defun qinvoke-method (object function-name &rest arguments)
  ;; for internal use
  (%qinvoke-method object function-name arguments))

(defmacro qget (object name)
  `(qrun* (%qget ,object ,(if (symbolp name)
                         (symbol-name name)
                         name))))

(defmacro qset (object &rest arguments)
  (assert (evenp (length arguments)))
  `(qrun* (%qset ,object (list ,@(let (name)
                                   (mapcar (lambda (x)
                                             (setf name (not name))
                                             (if (and name (symbolp x))
                                                 (symbol-name x)
                                                 x))
                                           arguments))))))

(defun qprocess-events (&optional exclude-user-input)
  (%qprocess-events exclude-user-input))

(defun exec-with-qt-restart ()
  ;; for internal use; for conditions in Slime during Qt event loop processing
  (loop (with-simple-restart (restart-qt-events "Restart Qt event processing.")
          (qexec))))

(defun qquit (&optional (exit-status 0) (kill-all-threads t))
  "args: (&optional (exit-status 0) (kill-all-threads t))
  alias: qq
  Terminates LQML. Use this function instead of ECL (ext:quit) to quit
  gracefully. Negative values for EXIT-STATUS will call C abort() instead of
  normal program exit."
  (declare (ignore kill-all-threads)) ; only here to be equivalent to EXT:QUIT 
  (assert (typep exit-status 'fixnum))
  (qrun* (%qquit exit-status)))

;;; android

(defun ensure-permissions (&rest permissions)
  (qrun* (%ensure-permissions permissions)))

(defun qlog (arg1 &rest args)
  "args: (arg1 &rest args)
  For debug messages. On android they can be captured with 'adb logcat'.
    (qlog 12)
    (qlog \"width\" w \"height\" h)
    (qlog \"x ~A y ~A\" x y)"
  (%qlog (if (and (stringp arg1)
                  (find #\~ arg1))
             (apply 'format nil arg1 args)
             (x:join (mapcar 'princ-to-string (cons arg1 args))))))

;;; ios

(defun disable-clipboard-menu (&optional (disable t))
  ;; see Qt sources hack in example 'cl-repl'
  (%disable-clipboard-menu disable))

;;; mobile ini

#+(or android ios)
(pushnew :mobile *features*)

#+mobile
(defvar *assets* #+android "assets:/lib/"
                 #+ios     "assets/")

#+ios
(defvar *local-assets* "local-assets/")

#+ios
(progn
  ;; adapt paths to iOS specific values
  (defvar *bundle-root*                *default-pathname-defaults*)
  (defvar *user-homedir-pathname-orig* (symbol-function 'user-homedir-pathname))

  (ext:package-lock :common-lisp nil)

  (defun cl:user-homedir-pathname (&optional host)
    (merge-pathnames "Library/" (funcall *user-homedir-pathname-orig* host)))

  (ext:package-lock :common-lisp t)

  (dolist (el '(("XDG_DATA_HOME"   . "")
                ("XDG_CONFIG_HOME" . "")
                ("XDG_DATA_DIRS"   . "")
                ("XDG_CONFIG_DIRS" . "")
                ("XDG_CACHE_HOME"  . ".cache")))
    (ext:setenv (car el) (namestring (merge-pathnames (cdr el)
                                                      (user-homedir-pathname))))))

#+mobile
(defun copy-asset-files (&optional (dir-name *assets*) origin)
  "Copy asset files to home directory."
  (flet ((directory-p (path)
           (x:ends-with "/" path))
         (translate (name)
           #+android
           (if (x:starts-with *assets* name)
               (subseq name (length *assets*))
               name)
           #+ios
           (namestring
	    (merge-pathnames (x:cc "../" (subseq name (length origin)))))))
    (ensure-directories-exist (translate dir-name))
    ;; note: both QDIRECTORY and QCOPY-FILE are prepared for accessing
    ;; APK asset files, which can't be accessed directly from Lisp
    (dolist (from (qdirectory dir-name))
      (if (directory-p from)
          (copy-asset-files from origin)
          (let ((to (translate from)))
            (when (probe-file to)
              (delete-file to))
            (unless (qcopy-file from to)
              (qlog "Error copying asset file: ~S" from)
              (return-from copy-asset-files))))))
  t)

#+mobile
(defun %ini-mobile ()
  ;; internal use, see 'main.cpp'
  (ext:install-bytecodes-compiler)
  #+ios
  (progn
    (setf *default-pathname-defaults* (user-homedir-pathname))
    (setf (logical-pathname-translations "SYS")
          (list (list "sys:**;*.*"
                      (merge-pathnames "**/*.*" (user-homedir-pathname)))))
    (setf (logical-pathname-translations "HOME")
          (list (list "home:**;*.*"
                      (merge-pathnames "**/*.*" (user-homedir-pathname))))))
  ;; copy all asset files on first startup of app
  ;; (note that PROBE-FILE is a hack here: for copying eventual, additional
  ;; asset files, either the whole directory "encodings/" needs to be removed
  ;; from within the app, or the app needs to be uninstalled first)
  (unless (probe-file (merge-pathnames "encodings/"))
    #+ios
    (flet ((dir (assets)
             (namestring (merge-pathnames assets *bundle-root*))))
      (let ((assets       (dir *assets*))
            (local-assets (dir *local-assets*)))
        (copy-asset-files assets assets)
        (copy-asset-files local-assets local-assets)))
    #+android
    (copy-asset-files)))

;;; alias

(defmacro alias (s1 s2)
  `(setf (fdefinition ',s1) (function ,s2)))

(alias qfun qinvoke-method)
(alias qrun qrun-on-ui-thread)
(alias qq   qquit)

