(in-package :qml)

(defvar *quick-view* nil) ; is set in 'lqml.cpp' on startup
(defvar *caller*     nil)
(defvar *root*       nil)
(defvar *root-item*  nil) ; see note in 'find-quick-item'

(defun string-to-symbol (name)
  (let ((upper (string-upcase name))
        (p (position #\: name)))
    (if p
        (find-symbol (subseq upper (1+ (position #\: name :from-end t)))
                     (subseq upper 0 p))
        (find-symbol upper))))

;;; function calls from QML

(defun print-js-readably (object)
  "Prints (nested) lists, vectors, T, NIL, floats in JS notation, which will be
  passed to JS 'eval()'."
  (if (and (not (stringp object))
           (vectorp object))
      (print-js-readably (coerce object 'list))
      (typecase object
        (cons
         (write-char #\[)
         (do ((list object (rest list)))
             ((null list) (write-char #\]))
           (print-js-readably (first list))
           (when (rest list)
             (write-char #\,))))
        (float
         ;; JS can't read 'd0' 'l0'
         (let ((str (princ-to-string object)))
           (x:when-it (position-if (lambda (ch) (find ch "dl")) str)
             (setf (char str x:it) #\e))
           (princ str)))
        (t
         (cond ((eql 't object)
                (princ "true"))
               ((eql 'nil object)
                (princ "false"))
               (t
                (prin1 object)))))))

(defun print-to-js-string (object)
  (with-output-to-string (*standard-output*)
    (princ "#<>") ; mark for passing to JS "eval()"
    (print-js-readably object)))

(defun qml-apply (caller function arguments)
  "Every 'Lisp.call()' or 'Lisp.apply()' function call in QML will call this
  function. The variable *CALLER* will be bound to the calling QQuickItem, if
  passed with 'this' as first argument to 'Lisp.call()' / 'Lisp.apply()'."
  (let* ((*caller* (if (zerop caller) ; don't change LET*
                       *caller*
                       (make-qobject caller)))
         (value (apply (string-to-symbol function)
                       arguments)))
    (if (stringp value)
        value
        (print-to-js-string value))))

;;; utils

(defun find-quick-item (object-name)
  "Finds the first QQuickItem matching OBJECT-NAME. Locally set *ROOT-ITEM* if
  you want to find items inside a specific item, like in a QML Repeater. See
  also note in sources."
  ;;
  ;; when to use *ROOT-ITEM*
  ;;
  ;; say you have a Repeater QML item with multiple instances of the same
  ;; QQuickItem. The children of those QQuickItems all have the same object
  ;; names, respectively. In order to access those child items, we need to
  ;; search in the specific item of the Repeater.
  ;;
  ;; So, we locally bind *ROOT-ITEM* in order to find a specific child item
  ;; inside the Repeater:
  ;;
  ;; (setf qml:*root-item* (q! |itemAt| ui:*repeater* 0)) ; (1) set
  ;; ;; everything we do here will only affect children of the first
  ;; ;; item in ui:*repeater* (see index 0 above)
  ;; (q< |text| ui:*edit*)
  ;; (setf qml:*root-item* nil)                           ; (2) reset
  ;;
  ;; N.B. we need SETF (instead of LET) because of the global var and threads
  ;;      (QRUN* is used internally here)
  ;;
  (let ((parent (or *root-item* (root-item))))
    (when (and parent (/= 0 (ffi:pointer-address parent)))
      (if (string= (qobject-name parent) object-name)
          parent
          (qfind-child parent object-name)))))

(defun quick-item (item/name)
  (cond ((stringp item/name)
         (find-quick-item item/name))
        ((qobject-p item/name)
         item/name)
        ((not item/name)
         (root-item))))

(defun children (item/name)
  "Like QML function 'children'."
  (qrun* (qchild-items (quick-item item/name))))

(defun reload ()
  "Reloads all QML files, clearing the cache."
  (qrun* (%reload)))

;;; get/set QML properties, call QML methods (through JS)

(defun qml-get (item/name property-name)
  "Gets QQmlProperty of either ITEM or first object matching NAME."
  (qrun* (%qml-get (quick-item item/name) property-name)))

(defun qml-set (item/name property-name value)
  "Sets QQmlProperty of either ITEM, or first object matching NAME.
  Returns T on success."
  (qrun* (%qml-set (quick-item item/name) property-name value)))

(defun qml-set-all (name property-name value)
  "Sets QQmlProperty of all objects matching NAME."
  (assert (stringp name))
  (qrun* (dolist (item (qfind-children (root-item) name))
           (qml-set item property-name value))))

(defmacro q! (method-name item/name &rest arguments)
  "Convenience macro for QML-CALL. Use symbol instead of string name."
  `(js ,item/name ,(symbol-name method-name) ,@arguments))

(defmacro q> (property-name item/name value)
  "Convenience macro for QML-SET. Use symbol instead of string name."
  `(qml-set ,item/name ,(symbol-name property-name) ,value))

(defmacro q< (property-name item/name)
  "Convenience macro for QML-GET. Use symbol instead of string name."
  `(qml-get ,item/name ,(symbol-name property-name)))

(defmacro q>* (property-name item/name value)
  "Convenience macro for QML-SET-ALL. Use symbol instead of string name."
  `(qml-set-all ,item/name ,(symbol-name property-name) ,value))

;;; JS

(defun js (item/name fun &rest arguments)
  "Evaluates a JS string, with 'this' bound to either ITEM, or first object
  matching NAME. Use this function instead of the (faster) QJS if you need to
  evaluate generic JS code, or for JS functions with default arguments."
  (qrun* (%js (quick-item item/name)
              (apply 'format nil
                     (format nil "~A(~A)"
                             fun
                             (x:join (loop repeat (length arguments) collect "~S") #\,))
                     (mapcar 'js-arg arguments)))))

(defun js-arg (object)
  "Used for arguments in function JS."
  (if (stringp object)
      object
      (with-output-to-string (*standard-output*)
        (print-js-readably object))))

(defmacro qjs (method-name item/name &rest arguments)
  `(qrun* (qfun (quick-item ,item/name)
                ,(if (symbolp method-name)
                     (symbol-name method-name)
                     method-name)
                ,@arguments)))

;;; apropos

(defun %to-qobject (x)
  (if (qobject-p x)
      x
      (quick-item x)))

(defun qapropos (name &optional class offset)
  (dolist (sub1 (%qapropos (%string-or-nil name) (%to-qobject class) offset))
    (format t "~%~%~A~%" (first sub1))
    (dolist (sub2 (rest sub1))
      (format t "~%  ~A~%~%" (first sub2))
      (dolist (sub3 (rest sub2))
        (let* ((par (position #\( sub3))
               (x (if par
                      (position #\Space sub3 :end par :from-end t)
                      (position #\Space sub3))))
          (format t "    ~A~A~%" (make-string (max 0 (- 15 x))) sub3)))))
  (terpri)
  nil)

(defun qapropos* (name &optional class offset)
  (%qapropos (%string-or-nil name) (%to-qobject class) offset))

