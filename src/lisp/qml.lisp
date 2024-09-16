(in-package :qml)

(defvar *quick-view* nil) ; QQuickView - is set in 'lqml.cpp' on startup
(defvar *engine*     nil) ; QQmlEngine - (see above)
(defvar *caller*     nil)
(defvar *root-item*  nil) ; see macro 'with-root-item'

(defun string-to-symbol (name)
  (let* ((upper (string-upcase name))
         (p (position #\: name))
         (pkg (find-package (subseq upper 0 p))))
    (if p
        (and pkg (find-symbol (subseq upper (1+ (position #\: name :from-end t)))
                              pkg))
        (find-symbol upper))))

(defun hex (integer)
  "args: (integer)
  Used internally, you should never need to call this explicitly.
  Since we only have floats in QML/JS, INTEGERs from Lisp are converted
  automatically to hex strings, if (1) populating an item model, or (2) passed
  with function QJS.

  Those hex strings are automatically converted back to a Lisp INTEGER when
  passed with 'Lisp.call()' or 'Lisp.apply()'.

  Important note: because of the automatic conversion of INTEGERs, you need to
  explicitly add '(float x)' in Lisp to values you don't want to be converted
  to hex strings, like drawing a line in a QML Canvas, or a FIXNUM integer
  which you want as number type in JS."
  (assert (integerp integer))
  (let ((*print-base* 16))
    (x:cc "#x" (princ-to-string integer))))

;;; function calls from QML

(defun qml-apply (caller function arguments)
  ;; Every 'Lisp.call()' or 'Lisp.apply()' function call in QML will call this
  ;: function. The variable *CALLER* will be bound to the calling QQuickItem,
  ;; if passed with 'this' as first argument to 'Lisp.call()' / 'Lisp.apply()'.
  ;;
  ;; Possible integers encoded as hex strings in JS (see function HEX above)
  ;; are automatically converted back to integers.
  ;;
  ;; Strings starting with ':' are assumed to be Lisp keywords, if composed by
  ;; alphanumeric/dash characters, and if they have a following argument.
  (let ((fun (string-to-symbol function))
        (*caller* (if (zerop caller)
                      *caller*
                      (qt-object caller))))
    (if (fboundp fun)
        (apply fun (loop :with len = (length arguments)
                         :for arg :in arguments
                         :for n :from 1 :to len
                         :collect (if (and (stringp arg)
                                           (or (x:starts-with "#x" arg)      ; integer
                                               (and (> (length arg) 1)
                                                    (char= #\: (char arg 0)) ; keyword
                                                    (every (lambda (ch)
                                                             (or (alphanumericp ch)
                                                                 (char= #\- ch)))
                                                           (subseq arg 1))
                                                    (< n len))))
                                      (or (ignore-errors (read-from-string arg))
                                          arg)
                                      arg)))
        (let ((msg (format nil "[LQML:error] Lisp.call(): ~S is undefined." function)))
          (when *break-on-errors*
            (break msg))
          (format *error-output* "~%~A~%" msg)))))

;;; utils

(defun find-quick-item (object-name)
  "args: (object-name)
  Finds the first QQuickItem matching OBJECT-NAME.
  See also WITH-ROOT-ITEM if you want to find items inside a specific item,
  like in a QML Repeater."
  (let ((parent (or *root-item* (root-item))))
    (when (and parent (/= 0 (qt-object-address parent)))
      (if (string= (qobject-name parent) object-name)
          parent
          (qfind-child parent object-name)))))

(defmacro with-root-item (root-item &body body)
  "args: (root-item)
  Say you have a Repeater QML item with multiple instances of the same
  QQuickItem. The children of those QQuickItems all have the same object names,
  respectively. In order to access those child items, we need to search in one
  specific item of the Repeater.
    (with-root-item (q! |itemAt| ui:*repeater* 0)
      (q< |text| ui:*edit*))"
  `(prog2
       (setf qml:*root-item* ,root-item)
       (progn
         ,@body)
     (setf qml:*root-item* nil)))

(defun quick-item (item/name)
  ;; for internal use
  (cond ((stringp item/name)
         (find-quick-item item/name))
        ((qt-object-p item/name)
         item/name)
        ((not item/name)
         (root-item))))

(defun reload ()
  "args: ()
  Reloads all QML files, clearing the cache."
  (qrun* (%reload)))

(defun view-status-changed (status)
  "args: (status)
  Redefine this function if you want to be notified on status changes of
  QQuickView, e.g. after a RELOAD.
    (defun qml:view-status-changed (status)
      (when (= 1 status)
        (populate-item-model)))"
  nil)

;;; get/set QML properties, call QML methods (through JS)

(defun qml-get (item/name property-name)
  ;; see Q<
  (qrun* (%qml-get (quick-item item/name) property-name)))

(defun qml-set (item/name property-name value)
  ;; see Q>
  (qrun* (%qml-set (quick-item item/name) property-name value)))

(defun qml-set-all (name property-name value)
  ;; see Q>*
  (assert (stringp name))
  (qrun* (dolist (item (qfind-children (root-item) name))
           (qml-set item property-name value))))

;;; convenience macros: re-arrange arguments to put the name first, and use
;;; a symbol instead of the string name, so we can use auto completion

(defmacro q< (property-name item/name)
  "args: (property-name item/name)
  Convenience macro for QML-GET. Use symbol instead of string name.
    (q< |text| *label*)
    (q< |font.pixelSize| *label*)"
  `(qml-get ,item/name ,(symbol-name property-name)))

(defmacro q> (property-name item/name value)
  "args: (property-name item/name value)
  Convenience macro for QML-SET. Use symbol instead of string name.
    (q> |text| *label* \"greetings!\")"
  `(qml-set ,item/name ,(symbol-name property-name) ,value))

(defmacro q>* (property-name item/name value)
  "args: (property-name item/name value)
  Convenience macro for QML-SET-ALL. Use symbol instead of string name. Sets
  given property of all items sharing the same 'objectName'."
  `(qml-set-all ,item/name ,(symbol-name property-name) ,value))

(defun js (item/name fun &rest arguments)
  ;; for internal use, see macro Q!
  (qrun* (%js (quick-item item/name)
              (apply 'format nil
                     (format nil "~A(~A)"
                             fun
                             (x:join (loop repeat (length arguments) collect "~S") #\,))
                     (mapcar 'js-arg arguments)))))

(defun print-js-readably (object)
  "Prints (nested) lists, vectors, T, NIL, floats in JS notation."
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

(defun js-arg (object)
  ;; for arguments in function JS
  (if (stringp object)
      object
      (with-output-to-string (*standard-output*)
        (print-js-readably object))))

(defmacro q! (method-name item/name &rest arguments)
  "args: (method-name item/name &rest arguments)
  For calling methods of QML items.
    (q! |requestPaint| *canvas*)"
  `(js ,item/name ,(symbol-name method-name) ,@arguments))

;;; JS calls

(defmacro qjs (function item/name &rest arguments)
  "args: (function item/name &rest arguments)
  Fast and convenient way to call JS functions defined in QML. You may pass
  up to 10 arguments of the following types:
  T, NIL, INTEGER, FLOAT, STRING, VECTOR of octets, ... and (nested) lists of
  mentioned arguments.

  For the complete list of supported types see 'marshal.cpp:toQVariant()'.
  A special use case is to populate an item model in QML (using a trivial JS
  glue code function) which expects a JS dictionary, see example below.

  N.B: Does not work with JS default arguments.
    (qjs |drawLine| *canvas* (float x1) (float y1) (float x2) (float y2))
    (qjs |addPlanet| *planets* (list :name \"Jupiter\" :src \"img/jupiter.png\"))"
  `(qrun* (qinvoke-method (quick-item ,item/name)
                          ,(if (symbolp function)
                               (symbol-name function)
                               function)
                          (list ,@arguments)
                          t))) ; qjs call

;;; apropos

(defun %to-qt-object (x)
  (if (qt-object-p x)
      x
      (quick-item x)))

(defun qapropos (string &optional qt-object/name offset)
  "args: (string &optional qt-object/name)
  Searches properties, methods, signals, slots for STRING in QObject (e.g.
  QQuickItem) passed as second argument. A QQuickItem can also be passed by
  its 'objectName'.
    (qapropos nil *canvas*)
    (qapropos \"color\")"
  (dolist (sub1 (%qapropos (%string-or-nil string) (%to-qt-object qt-object/name) offset))
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

(defun qapropos* (name &optional qt-object/name offset)
  "args: (name &optional qt-object/name)
  Similar to QAPROPOS, returning the results as nested list."
  (%qapropos (%string-or-nil name) (%to-qt-object qt-object/name) offset))

