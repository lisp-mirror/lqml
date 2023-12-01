;;; this is a port of example 'REPL' from eql5 mobile
;;;
;;; it needs a small plugin (see 'cpp/') to implement some functionality not
;;; available in QML, but needed for syntax highlighting etc.

(in-package :editor)

(defvar *text-color*              "black")
(defvar *background-color*        "white")
(defvar *selected-text-color*     "white")
(defvar *selection-color*         "firebrick")
(defvar *parenthesis-color*       "lightslategray")
(defvar *string-color*            "saddlebrown")
(defvar *comment-color*           "lightslategray")
(defvar *lisp-keyword-color*      "#c05050")
(defvar *lqml-keyword-color*      "#5050c0")
(defvar *keyword-color*           "#409090")

(defvar *output-text-color*       "black")
(defvar *output-background-color* "lavender")
(defvar *output-string-color*     "saddlebrown")
(defvar *output-value-color*      "#2020ff")
(defvar *output-trace-color*      "darkmagenta")
(defvar *output-error-color*      "red")

(defvar *button-color*            "#e0e0e0")
(defvar *button-text-color*       "#26282a")
(defvar *button-opacity*          0.12) ; arrow and paren buttons only

(defvar *cursor-color*            "blue")

(defun apply-colors ()
  (qt:set-format qt:*cpp* *lisp-keyword-format*
                 (list :color *lisp-keyword-color*
                       :bold t))
  (qt:set-format qt:*cpp* *lqml-keyword-format*
                 (list :color *lqml-keyword-color*
                       :bold t))
  (qt:set-format qt:*cpp* *keyword-format*
                 (list :color *keyword-color*
                       :bold t))
  (qt:set-format qt:*cpp* *comment-format*
                 (list :color *comment-color*
                       :italic t))
  (dolist (item/color (list (cons ui:*edit*         *text-color*)
                            (cons ui:*rect-edit*    *background-color*)
                            (cons ui:*command*      *text-color*)
                            (cons ui:*rect-command* *background-color*)
                            (cons ui:*rect-output*  *output-background-color*)))
    (q> |color| (car item/color)
        (cdr item/color)))
  (dolist (item (list ui:*edit* ui:*command*))
    (q> |selectedTextColor| item *selected-text-color*)
    (q> |selectionColor| item *selection-color*))
  (dolist (item (list (root-item) ui:*clipboard-menu*))
    (q> |palette.button| item *button-color*)
    (q> |palette.buttonText| item *button-text-color*))
  (dolist (button (list ui:*paren-open* ui:*paren-close*))
    (q> |opacity| button *button-opacity*)
    (q> |icon.color| button *button-text-color*))
  (dolist (button (list ui:*left* ui:*right* ui:*up* ui:*down*))
    (q> |opacity| button *button-opacity*)
    (q> |palette.windowText| button *button-text-color*))
  (q> |cursorColor| ui:*main* *cursor-color*)
  (unless (zerop (q< |length| ui:*edit*))
    ;; apply to editor
    (q! |selectAll| ui:*edit*)
    (q! |cut| ui:*edit*)
    (q! |paste| ui:*edit*)))

(defvar *lisp-keyword-format*    nil)
(defvar *lqml-keyword-format*    nil)
(defvar *keyword-format*         nil)
(defvar *comment-format*         nil)

(defvar *lisp-match-rule*        nil)
(defvar *keyword-match-rule*     nil)

(defvar *highlighter-edit*       nil)
(defvar *highlighter-command*    nil)

(defvar *qml-document-edit*      nil)
(defvar *qml-document-command*   nil)

(defvar *package-char-dummy*     #\$)
(defvar *separator*              "#||#")
(defvar *current-depth*          0)
(defvar *current-keyword-indent* 0)
(defvar *cursor-indent*          0)
(defvar *file*                   nil)

(defmacro enum (&rest names/values)
  `(progn
     ,@(mapcar (lambda (n/v) `(defconstant ,(car n/v) ,(cdr n/v)))
               names/values)))

(defun read-file (file)
  (with-open-file (s file)
    (let ((str (make-string (file-length s))))
      (read-sequence str s)
      str)))

(defun ini-highlighters ()
  (setf *lisp-keyword-format* (qt:make-object qt:*cpp* "QTextCharFormat")
        *lqml-keyword-format* (qt:make-object qt:*cpp* "QTextCharFormat")
        *keyword-format*      (qt:make-object qt:*cpp* "QTextCharFormat")
        *comment-format*      (qt:make-object qt:*cpp* "QTextCharFormat")
        *lisp-match-rule*     "[(']:*[^ )]+"
        *keyword-match-rule*  "[(': ]?[*:&][a-z1-9\\-*]*")
  (setf *highlighter-edit*    (qt:make-object qt:*cpp* "QSyntaxHighlighter" *qml-document-edit*)
        *highlighter-command* (qt:make-object qt:*cpp* "QSyntaxHighlighter" *qml-document-command*)))

(defun read* (string &optional (start 0))
  (flet ((no-package-colons (str)
           (let ((str* (copy-seq str))
                 (ex #\Space))
             (dotimes (i (length str*))
               (let ((ch (char str* i)))
                 (when (and (char= #\: ch)
                            (char/= #\# ex))
                   (setf (char str* i) *package-char-dummy*))
                 (setf ex ch)))
             str*)))
    (let ((*package* #.(find-package :qml)))
      (multiple-value-bind (exp x)
          (ignore-errors
           (read-from-string (no-package-colons string)
                             nil nil :start start :preserve-whitespace t))
        (values exp x)))))

(defun end-position (string)
  (multiple-value-bind (x end)
      (read* string)
    (declare (ignore x))
    (when (numberp end)
      end)))

;;; syntax highlighting

(enum
 (+no-value+   . -1)
 (+in-comment+ . 1)
 (+in-string+  . 2))

(defun highlight-block (highlighter text)
  ;; CL, LQML functions and macros
  (multiple-value-bind (i end)
      (ppcre:scan *lisp-match-rule* text)
    (x:while i
      (let* ((len (- end i))
             (kw* (string-left-trim "(" (subseq text (1+ i) (+ i len))))
             (kw  (x:if-it (position #\: kw* :from-end t)
                           (subseq kw* (1+ x:it))
                           kw*)))
        (flet ((set-format (frm)
                 (qt:set-format qt:*cpp* highlighter
                                (list (1+ i) (1- len) frm))))
          (cond ((gethash kw *lisp-keywords*)
                 (set-format *lisp-keyword-format*))
                ((find kw *lqml-keywords-list* :test 'string=)
                 (set-format *lqml-keyword-format*))))
        (multiple-value-setq (i end)
          (ppcre:scan *lisp-match-rule* text :start (+ i len))))))
  ;; CL keywords etc.
  (multiple-value-bind (i end)
      (ppcre:scan *keyword-match-rule* text)
    (let ((extra "(' "))
      (x:while i
        (let* ((len (- end i))
               (kw (subseq text i (+ i len))))
          (when (gethash (string-left-trim extra kw) *keywords*)
            (let ((skip (find (char kw 0) extra)))
              (unless (and (not skip) (plusp i))
                (qt:set-format qt:*cpp* highlighter
                               (list (if skip (1+ i) i)
                                     (if skip (1- len) len)
                                     *keyword-format*)))))
          (multiple-value-setq (i end)
            (ppcre:scan *keyword-match-rule* text :start (+ i len)))))))
  ;; comments, strings, parenthesis
  (flet ((set-comment-format (pos len)
           (qt:set-format qt:*cpp* highlighter
                          (list pos len *comment-format*)))
         (set-color (pos len color)
           (qt:set-format qt:*cpp* highlighter
                          (list pos len color)))
         (set-state (state)
           (qt:set-current-block-state qt:*cpp* highlighter
                                       state)))
    (let* ((ex #\Space)
           (state (qt:current-block-state qt:*cpp* highlighter))
           (prev-state (qt:previous-block-state qt:*cpp* highlighter))
           (in-string (= +in-string+ prev-state))    ; multi line strings
           (in-comment (= +in-comment+ prev-state))) ; multi line comments
      (set-state (if (x:empty-string text) prev-state +no-value+))
      (dotimes (i (length text))
        (let ((ch (char text i)))
          (unless (char= #\\ ex)
                  ;; multi line comment
            (cond ((or (and (char= #\# ex)
                            (char= #\| ch))
                       (and (zerop i)
                            in-comment))
                   (let ((len (x:if-it (search "|#" (subseq text i))
                                       (+ x:it (if in-comment 2 3))
                                       (progn
                                         (set-state +in-comment+)
                                         (length text)))))
                     (set-comment-format (max (1- i) 0) len)
                     (incf i (1- len)))
                   (trigger-force-repaint)
                   (setf ch #\Space))
                  ;; single/multi line string
                  ((or (char= #\" ch)
                       (and (zerop i)
                            in-string))
                   (let ((len (x:if-it (end-position (if in-string (x:cc "\"" text) (subseq text i)))
                                       (if in-string (1- x:it) x:it)
                                       (progn
                                         (set-state +in-string+)
                                         (length text)))))
                     (set-color i len *string-color*)
                     (incf i (1- len)))
                   (setf ch #\Space))
                  ;; parens
                  ((find ch "()")
                   (set-color i 1 *parenthesis-color*))
                  ;; single line comment
                  ((char= #\; ch)
                   (set-comment-format i (- (length text) i))
                   (return))))
          (setf ex ch)))
      (when (/= state (qt:current-block-state qt:*cpp* highlighter))
        (trigger-force-repaint)))))

(let (start)
  (defun trigger-force-repaint ()
    ;; trigger max. every 300 ms
    (when (string= ui:*edit* (active-edit))
      (unless start
        (setf start (get-internal-real-time))
        (qsingle-shot 100 'force-repaint))
      (when (> (- (get-internal-real-time) start)
               300)
        (setf start nil)))))

(defun force-repaint ()
  ;; workaround for missing repaint bug after multi line highlight changes
  (let ((pos (q< |cursorPosition| ui:*edit*))
        (len (q< |length| ui:*edit*)))
    (when (plusp len)
      (q> |skipEnsureVisible| ui:*main* t)
      (q! |select| ui:*edit* (1- len) len)
      (q> |cursorPosition| ui:*edit* pos)
      (q> |skipEnsureVisible| ui:*main* nil))))

;;; auto completion: start when 2 spaces have been inserted in less than 500 ms

(let ((pos 0))
  (defun edit-contents-changed ()
    (let ((pos* (q< |cursorPosition| ui:*edit*)))
      (when (= pos* (1+ pos))
        (qlater (lambda () (contents-changed *qml-document-edit* (1- pos)))))
      (setf pos pos*))))

(let ((pos 0))
  (defun command-contents-changed ()
    (let ((pos* (q< |cursorPosition| ui:*command*)))
      (when (= pos* (1+ pos))
        (qlater (lambda () (contents-changed *qml-document-command* (1- pos)))))
      (setf pos pos*))))

(let ((space-count 0))
  (defun contents-changed (document position)
    (when (and (plusp position)
               (char= #\Space (qt:character-at qt:*cpp* document
                                               position)))
      (qsingle-shot 500 (lambda () (setf space-count 0)))
      (when (= 2 (incf space-count))
        (let ((ch (qt:character-at qt:*cpp* document
                                   (- position 2))))
          (when (alphanumericp ch)
            (let ((start (- position 3))
                  (text (list ch)))
              (x:while (and (not (minusp start))
                            (or (alphanumericp (setf ch (qt:character-at qt:*cpp* document
                                                                         start)))
                                (find ch "-:&*")))
                (decf start)
                (push ch text))
              (search-completion (coerce text 'string)))))))))

(defun search-completion (short)
  (let ((p (position #\: short)))
    (when (and p (plusp p))
      (setf short (subseq short (1+ p)))))
  (let* ((edit (active-edit))
         (pos (q< |cursorPosition| edit)))
    (q! |remove| edit (- pos 2) pos) ; remove the 2 spaces
    (x:when-it (complete-symbol short)
      (setf pos (q< |cursorPosition| edit))
      (let ((pos-2 (- pos (length short))))
        (q! |remove| edit pos-2 pos)
        (q! |insert| edit pos-2 x:it)))))

(defvar *qt-regexp* nil)

(defun ensure-qt-regexp ()
  (unless *qt-regexp*
    (setf *qt-regexp* (qt:make-object qt:*cpp* "QRegularExpression"))))

(defun complete-symbol (short)
  "Works only for fixed set of CL symbols / CL keywords / global CL variables /
  LQML symbols and respective abbreviations."
  (if (find #\- short)
      ;; complete an abbreviation; example: "m-v-b" => "multiple-value-bind"
      ;; QRegularExpression instead of 'ppcre' because of 1000+ repeated calls
      (progn
        (ensure-qt-regexp)
        (setf short (x:string-substitute "\\*" "*" short))
        (qt:set-pattern qt:*cpp* *qt-regexp*
                        (x:cc (x:string-substitute "[a-z1-9:&*]*-" "-" short)
                              "[a-z1-9\\-*]*"))
        (dolist (names (list *lisp-keywords-list*
                             *keywords-list*
                             *lqml-keywords-list*))
          (dolist (name names)
            (when (qt:exact-match qt:*cpp* *qt-regexp*
                                  name)
              (return-from complete-symbol name)))))
      ;; complete as far as unambiguous; return full name if unique
      (let (matches)
        (dolist (names (list *lisp-keywords-list*
                             *keywords-list*
                             *lqml-keywords-list*))
          (dolist (name names)
            (when (x:starts-with short name)
              (push name matches))))
        (when matches
          (if (rest matches)
              (let ((i1 (1+ (length short)))
                    (i2 (apply 'min (mapcar 'length matches))))
                (do ((i i1 (1+ i)))
                    ((> i i2) (subseq (first matches) 0 (1- i)))
                  (let ((start (subseq (first matches) 0 i)))
                    (unless (every (lambda (str) (x:starts-with start str))
                                   matches)
                      (return-from complete-symbol (subseq start 0 (1- (length start))))))))
              (first matches))))))

;;; the following are workarounds because QML 'Keys' doesn't work on all devices

(defun current-line ()
  (let* ((rect (q< |cursorRectangle| ui:*edit*))
         (y (second rect))
         (h (fourth rect)))
    (when (plusp h)
      (1- (truncate (/ y h))))))

(let ((old 1))
  (defun edit-line-count-changed (new)
    (unless *pasting*
      (when (> new old)
        (x:when-it (current-line)
          (qlater (lambda () (return-pressed x:it)))))) ; QLATER: avoid race condition
    (setf old new))
  (defun reset-line-count ()
    (setf old 1)))

(let ((old 1))
  (defun command-line-count-changed (new)
    (if (> new old)
        (let ((line (remove #\Newline (q< |text| ui:*command*))))
          (q! |clear| ui:*command*)
          (eval-expression line)
          (setf old 1))
    (setf old new))))

;;; auto-indent

(defparameter *two-spaces-indent-symbols*
 '(case ccase ecase ctypecase etypecase handler-bind handler-case catch
   defclass defgeneric defstruct defun defmacro defmethod destructuring-bind do
   do* dolist dotimes do-all-symbols do-external-symbols do-symbols flet labels
   lambda let let* loop multiple-value-bind prog progn prog1 prog2 progv qlet
   typecase unless when with-open-file with-output-to-string
   with-input-from-string qml::do-string qml::do-with qml::when-it
   qml::when-it* qml::while qml::while-it))

(defparameter *four-spaces-indent-symbols*
 '(multiple-value-bind prog1 prog2))

(defun auto-indent-spaces (kw)
  (when (symbolp kw)
    (let* ((name (symbol-name kw))
           (p (x:if-it (position *package-char-dummy* name :from-end t)
                       (1+ x:it)
                       0))
           (symbol (read* (subseq name p))))
      (cond ((find symbol *four-spaces-indent-symbols*)
             4)
            ((find symbol *two-spaces-indent-symbols*)
             2)))))

(defun cut-comment (line)
  (let ((ex #\Space))
    (dotimes (i (length line))
      (let ((ch (char line i)))
        (when (and (char= #\; ch)
                   (char/= #\\ ex))
          (return-from cut-comment (subseq line 0 i)))
        (setf ex ch))))
  line)

(defun last-expression-indent (line)
  (flet ((str-subst (a1 b1 a2 b2 str)
           (x:string-substitute a1 b1 (x:string-substitute a2 b2 str))))
    (let* ((line* (string-right-trim " " (str-subst "  " "\\("
                                                    "  " "\\)"
                                                    (cut-comment line))))
           (open  (position #\( line* :from-end t))
           (space (when open (position #\Space line* :start open)))
           (one   (and open (not space) (not (x:ends-with ")" line*)))))
      (if one
          (1+ open)
          (or (position #\Space (if space line* line) :test 'char/= :start (or space 0))
              0)))))

(defvar *indentation-already-calculated* nil)

(defun update-indentations (code pos return-pressed)
  (flet ((pos-newline (&optional (start 0))
           (when start
             (or (position #\Newline code :start start) (length code)))))
    (let* ((pos-keyword    (paren-match-index code -1))
           (pos-local      (paren-match-index code -3))
           (keyword-indent (x:when-it (pos-newline pos-keyword) (- x:it pos-keyword 1)))
           (code1          (reverse (subseq code 0 pos-keyword)))
           (keyword        (read* code1))
           (auto-indent    (auto-indent-spaces keyword))
           (in-local       (find (read* (reverse (subseq code 0 pos-local)))
                                 '(flet labels macrolet)))
           (local-indent   (x:when-it (and in-local (pos-newline pos-local)) (- x:it pos-local 1))))
      (setf *current-depth*          (or local-indent
                                         (if auto-indent (or keyword-indent pos) pos))
            *current-keyword-indent* (if local-indent
                                         (+ 5 (length (symbol-name in-local)))
                                         (or auto-indent 0)))
      (when (and (find keyword *four-spaces-indent-symbols*)
                 (>= (count #\Newline code1)
                     (if (eql 'prog2 keyword) 2 1)))
        (decf *current-keyword-indent* 2)
        (setf *indentation-already-calculated* t))
      (if (and return-pressed
               (not (eql 'loop keyword)))
          (unless *indentation-already-calculated*
            (let* ((line  (reverse (subseq code 0 (pos-newline))))
                   (par1  (position #\( line :from-end t))
                   (par2  (position #\) line :from-end t))
                   (par   (and par1 (or (not par2) (> par1 par2)) par1))
                   (spc   (position #\Space line :start (or par 0)))
                   (depth (position-if-not (lambda (ch) (char= #\Space ch)) line
                                           :start (or spc 0))))
              (when (and depth par (< depth par))
                (setf depth par))
              (when (and par
                         (not spc)
                         (zerop *current-keyword-indent*))
                (setf depth (1+ par)))
              (when depth
                (setf *current-depth* (if (char= #\; (char line 0)) 0 depth))
                (when spc
                  (setf *current-keyword-indent* 0)))))
          (setf *indentation-already-calculated* t)))))

(defun indentation (line)
  (if (x:empty-string (string-trim " " line))
      0
      (+ *current-depth* *current-keyword-indent*)))

(defun return-pressed (line-number)
  (let* ((text-block (qt:find-block-by-line-number qt:*cpp* *qml-document-edit*
                                                   line-number))
         (line (qt:text qt:*cpp* text-block)))
    ;; update current indentation when no ')' has been inserted
    (right-paren text-block line :return-pressed)
    (setf *indentation-already-calculated* nil)
    (let ((spaces (indentation line)))
      (unless (zerop spaces)
        (qlater (lambda ()
                  (q! |insert| ui:*edit*
                      (q< |cursorPosition| ui:*edit*)
                      (make-string spaces)))))))
  (setf *current-depth*          0
        *current-keyword-indent* 0))

;;; paren highlighting

(defvar *left-paren-indent*  nil)
(defvar *closing-all-parens* nil)

(defun code-parens-only (code &optional right)
  "Substitute all non code related parenthesis with a space character."
  (let ((ex #\Space)
        (len (length code))
        comment in-string)
    (dotimes (i len)
      (let* ((i* (if right (- len i 1) i))
             (ch (char code i*)))
        (cond ((char= #\\ ex)
               (when (find ch "();\"")
                 (setf (char code i*) #\Space)))
              ((and (not in-string) (char= #\; ch))
               (setf comment t))
              ((char= #\Newline ch)
               (setf comment nil))
              ((char= #\" ch)
               (setf in-string (not in-string)))
              ((or comment in-string)
               (when (find ch "()")
                 (setf (char code i*) #\Space))))
        (setf ex ch))))
  code)

(defun paren-match-index (code &optional (n 0))
  (dotimes (i (length code))
    (let ((ch (char code i)))
      (case ch
        (#\( (incf n))
        (#\) (decf n))))
    (when (zerop n)
      (return-from paren-match-index i))))

(defun current-document ()
  (if (q< |activeFocus| ui:*edit*)
      *qml-document-edit*
      *qml-document-command*))

(defun code-region (text-block curr-line &optional right)
  (let ((max (qt:line-count qt:*cpp* (current-document))))
    (with-output-to-string (s)
      (write-line (if right (nreverse curr-line) curr-line) s)
      (do* ((n (qt:block-number qt:*cpp* text-block) (+ n (if right -1 1)))
            (curr-block (funcall (if right 'qt:previous 'qt:next) qt:*cpp* text-block)
                        (funcall (if right 'qt:previous 'qt:next) qt:*cpp* curr-block))
            (text (qt:text qt:*cpp* curr-block) (qt:text qt:*cpp* curr-block)))
           ((or (if right (zerop n) (= n max))
                (x:empty-string (string-trim " " text))))
        (write-line (if right (nreverse text) text) s)))))

(defun left-right-paren (right text-block curr-line &optional return-pressed)
  (let ((code (code-parens-only (code-region text-block curr-line right) right)))
    (x:when-it (paren-match-index code)
      (when right
        (update-indentations code (- (position #\Newline code :start x:it) x:it 1) return-pressed))
      x:it)))

(defun right-paren (text-block curr-line &optional return-pressed)
  (unless (x:ends-with "\\)" curr-line)
    (left-right-paren :right text-block curr-line return-pressed)))

(defun active-edit ()
  (if (q< |activeFocus| ui:*edit*)
      ui:*edit*
      ui:*command*))

(defun cursor-position-changed (text-cursor)
  (unless *pasting*
    (let* ((text-block (qt:block* qt:*cpp* text-cursor))
           (line (qt:text qt:*cpp* text-block))
           (pos (qt:position-in-block qt:*cpp* text-cursor)))
      (setf *cursor-indent* pos)
      (when (and (plusp pos)
                 (char= #\) (char line (1- pos))))
        (show-matching-paren text-block
                             (subseq line 0 pos)
                             (qt:position* qt:*cpp* text-cursor))))))

(let ((ex-from -1))
  (defun show-matching-paren (text-block line pos)
    (x:when-it (right-paren text-block line)
      (let* ((edit (active-edit))
             (set-y (string= edit ui:*edit*))
             (from (- pos x:it 1))
             (color (q< |selectionColor| edit)))
        (when (/= from ex-from)
          (setf ex-from from)
          (qsingle-shot 500 (lambda () (setf ex-from -1)))
          (let ((content-y (when set-y (q< |contentY| ui:*flick-edit*))))
            (q> |selectionColor| edit "gray")
            (q! |select| edit from (1+ from))
            (setf *left-paren-indent*
                  (> (first (q! |positionToRectangle| edit from))
                     4)) ; pixel indent of QML "command"
            (unless *closing-all-parens*
              (dotimes (i 2)
                (qprocess-events :exclude-user-input)
                (sleep 0.02)))
            (qlater (lambda ()
                      (q> |cursorPosition| edit pos)
                      (q> |selectionColor| edit color)
                      (when set-y
                        (q> |contentY| ui:*flick-edit* content-y))
                      (when *closing-all-parens*
                        (qlater 'do-close-all-parens))))))))))

(let (n)
  (defun close-all-parens ()
    (setf n                    25 ; limit (to be safe on tilts)
          *closing-all-parens* t)
    (insert-closing-paren))
  (defun do-close-all-parens ()
    (if (and *left-paren-indent*
             (plusp (decf n)))
        (insert-closing-paren)
        (setf *closing-all-parens* nil)))
  (defun insert-closing-paren ()
    (qsingle-shot 50 (lambda () (insert ")")))))

;;; find text

(defvar *plain-text-search* nil
  "If set to T, FIND-TEXT will search for plain text instead of a regular
  expression.")

(defun find-text (text)
  "Selects a regular expression (or plain text) if found, returning both the
  matched text and the position; restarts from top after last match."
  ;; can't use cl-ppcre here, because Qt method expects a QRegularExpression;
  ;; ensure running on UI thread, required for return values of 'qt:' methods
  (qrun* (unless *plain-text-search*
           (ensure-qt-regexp)
           (qt:set-pattern qt:*cpp* *qt-regexp*
                           text))
         (let* ((result (qt:find* qt:*cpp* *qml-document-edit*
                                  (if *plain-text-search*
                                      text
                                      *qt-regexp*)
                                  (q< |cursorPosition| ui:*edit*)))
                (start (first result))
                (end (second result)))
           (if (minusp start)
               (unless (zerop (q< |cursorPosition| ui:*edit*))
                 (q! |deselect| ui:*edit*)
                 (q> |cursorPosition| ui:*edit* 0)
                 (princ "from top")
                 (find-text text))
               (progn
                 (q! |select| ui:*edit* start end)
                 (values (third result)
                         start))))))

;;; eval

(defvar *ex-cmd* nil)

(defun feed-top-level (text)
  (when eval:*eval-thread*
    (if (mp:process-active-p eval:*eval-thread*)
        (progn
          (print-eval-output :trace ";; killing old eval process")
          (eval* ":k"))
        (setf eval:*eval-thread* nil)))
  (unless eval:*eval-thread*
    (eval:feed-top-level text)))

(defun eval* (text)
  (if (find #\Newline text)
      (feed-top-level text)
      (let ((text* (string-trim " " text)))
        (flet ((cmd (str)
                 (string-equal str text*)))
          (cond ((x:empty-string text*)
                 (when (and *ex-cmd*
                            (x:starts-with "(editor:find-text " *ex-cmd*))
                   (feed-top-level *ex-cmd*)))
                ((cmd ":k")
                 (if eval:*eval-thread*
                     (progn
                       (when (mp:process-active-p eval:*eval-thread*)
                         (mp:process-kill eval:*eval-thread*))
                       (setf eval:*eval-thread* nil)
                       (eval:set-eval-state nil)
                       (eval:clear-buffers)
                       (print-eval-output :error ":KILLED"))
                     (print-eval-output :values "kill: eval thread not running")))
                (t
                 (let ((cmd (cond ((cmd ":h")
                                   "(dialogs:help)")
                                  ((cmd ":s")
                                   "(qml:start-swank)")
                                  ((cmd ":q")
                                   "(qml:quicklisp)")
                                  ((cmd ":c")
                                   "(progn (qml:q! |clear| ui:*output-model*) (values))")
                                  ((cmd "*")
                                   (format nil "(progn~%  (editor::set-clipboard-text (prin1-to-string *))~%  *)"))
                                  ((cmd ":w")
                                   "(s-http-server:start)")
                                  ((cmd ":ws")
                                   "(s-http-server:stop)")
                                  ((x:starts-with ":? " text*)
                                   (format nil "(editor:find-text ~S)" (subseq text* #.(length ":? ")))))))
                   (setf *ex-cmd* cmd)
                   (feed-top-level (or cmd text)))))))))

(defun append-output (x &key (color *output-value-color*) bold line rich-text)
  "Prints X (of any type) to the output window. Optionally pass the following
  &key arguments: name of :color, T for :bold / :line (separating line) /
  :rich-text (text is subset of html). The ouput is printed immediately
  (important for longer running tasks)."
  (qjs |appendOutput| ui:*output-model*
       (list :text      (if (stringp x) x (prin1-to-string x))
             :color     color
             :bold      bold
             :line      line
             :rich-text rich-text)))

(qml::alias pr append-output)

(defun ensure-output-visible () ; called from QML
  (qsingle-shot 250 (lambda ()
                      (q> |contentX| ui:*output* 0)
                      (q! |positionViewAtEnd| ui:*output*))))

(defun print-eval-output (type text)
  (let ((color (case type
                 (:output *output-string-color*)
                 (:values (if (string= ":UNCAUGHT-EXCEPTION" text)
                              *output-error-color*
                              *output-value-color*))
                 (:trace  *output-trace-color*)
                 (:error  *output-error-color*)
                 (t       *output-text-color*)))
        (bold (not (eql :expression type)))
        (line (eql :expression type)))
    (append-output (if (eql :values type)
                       (x:string-substitute #.(string #\Newline) *separator* text)
                       text)
                   :color color
                   :bold  bold
                   :line  line)))

(defun eval-expression* ()
  (qlater 'eval-expression)) ; QLATER for key release event

(defun eval-expression (&optional single (history t))
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (or single (q< |text| ui:*edit*)))))
    (eval* text)
    (when (and single history)
      (history-add text)))
  (q> |text| ui:*query-text* ""))

(defun eval-single-expression ()
  (eval-expression *selected-text* nil))

;;; command history

(defvar *history*       (make-array 0 :adjustable t :fill-pointer t))
(defvar *history-index* nil)
(defvar *history-file*  (merge-pathnames ".repl-history" (user-homedir-pathname)))
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
    (unless (x:empty-string line)
      (unless out
        (history-ini))
      (let ((len (length *history*)))
        (when (or (zerop len)
                  (string/= line (aref *history* (1- len))))
          (vector-push-extend line *history*)
          (write-line line out)
          (finish-output out)))
      (setf *history-index* (length *history*)))) ; 1 after last
  (defun history-move (dir)
    ;; 'dir' is a string, to be callable from QML
    (unless out
      (history-ini))
    (when (and *history-index*
               (plusp (length *history*)))
      (setf *history-index* (if (string= "back" dir)
                                (max (1- *history-index*) 0)
                                (min (1+ *history-index*) (1- (length *history*)))))
      (let ((text (aref *history* *history-index*)))
        (q> |text| ui:*command* text)
        (qlater (lambda ()
                  (q> |cursorPosition| ui:*command*
                      (- (length text) (if (x:ends-with ")" text) 1 0)))))
        text))))

;;; etc.

(defun change-font (to &optional (steps 1))
  (let ((size (+ (q< |font.pixelSize| ui:*edit*)
                 (* steps
                    (if (q< |small| nil) 1 2)
                    (if (eql :bigger to) 1 -1)))))
    (q> |font.pixelSize| ui:*edit*    size)
    (q> |font.pixelSize| ui:*command* size)
    (q> |fontSize|       ui:*output*  size)))

(defun set-font (&rest files)
  "Set custom font from local file. You should pass at least 2 files, the
  regular and the bold one (of same font family).
  Example (to put in '~/.eclrc'):
    (set-font \"/sdcard/fonts/font-regular.ttf\"
              \"/sdcard/fonts/font-bold.ttf\")"
  (let (font-family)
    (dolist (file files)
      (assert (probe-file file))
      (let ((name (qjs |loadFont| ui:*main*
                       (format nil "file://~A" file))))
        (unless font-family
          (setf font-family name))))
    (q> |font.family| ui:*edit*    font-family)
    (q> |font.family| ui:*command* font-family)
    (q> |fontFamily|  ui:*output*  font-family)))

(defun clear ()
  (q! |clear| ui:*edit*)
  (setf *file* nil)
  (setf *current-keyword-indent* 0
        *cursor-indent*          0)
  (reset-line-count))

(defun insert (text)
  (let ((edit (active-edit)))
    ;; QLATER: prevent blocking on fast, repeated calls
    (qlater (lambda ()
              (q! |insert| edit
                  (q< |cursorPosition| edit)
                  text)))))

;;; open file

(defun open-file ()
  (save-changes :confirm)
  (dialogs:get-file-name 'do-open-file))

(defun do-open-file (&optional file)
  (let ((file-name (namestring (or file dialogs:*file-name*))))
    (unless (x:empty-string file-name)
      (if (probe-file file-name)
          (let ((file-type (pathname-type file-name)))
            (cond ((x:starts-with "fas" file-type)
                   ;; wait for dialog to be hidden
                   (qsingle-shot 150 (lambda ()
                                       (eval* (format nil "(load ~S)" file-name)))))
                  ((string= "qml" file-type)
                   (qml-item-from-file file-name))
                  (t
                   (setf *file* file-name)
                   (q> |text| ui:*edit* (read-file *file*))
                   (reset-line-count))))
          (qjs |message| ui:*dialogs*
               (format nil "File does not exist:~%~%~S" file-name)))))
  (qsingle-shot 250 (lambda () (q! |forceActiveFocus| ui:*edit*))))

;;; save-file

(defun save-to-file (file)
  (ensure-directories-exist file)
  (unless (ignore-errors
           (with-open-file (s file :direction :output
                             :if-exists :supersede)
             (write-sequence (q< |text| ui:*edit*) s)
             (qt:clear-undo-redo-stacks qt:*cpp* *qml-document-edit*))
           t)
    (qjs |message| ui:*dialogs*
         (format nil "File not saved. Please ensure you have write permissions in:~%~%~S"
                 file))))

(defun confirm-dialog (title text callback)
  (qjs |confirm| ui:*dialogs*
       title text (x:callback-name callback)))

(defun save-file ()
  (when (q< |canUndo| ui:*edit*)
    (if *file*
        (confirm-dialog "Save?"
                        (format nil "Save to opened file, overwriting it?<br><br>~S<br>"
                                *file*)
                        'safe-file-confirmed?)
        (safe-file-confirmed? nil))))

(defun safe-file-confirmed? (save)
  (if save
      (save-to-file *file*)
      (dialogs:get-file-name 'do-save-file t)))

(defun do-save-file ()
  (unless (x:empty-string dialogs:*file-name*)
    (let ((type (pathname-type dialogs:*file-name*)))
      (when (and (string/= ".eclrc" (pathname-name dialogs:*file-name*))
                 (or (not type)
                     (not (find type '("lisp" "lsp" "asd" "exp" "sexp" "qml")
                                :test 'string-equal))))
        (setf dialogs:*file-name* (x:cc dialogs:*file-name* ".lisp"))))
    (if (probe-file dialogs:*file-name*)
        (confirm-dialog "Overwrite?"
                        (format nil "File already exists; overwrite?<br><br>~S<br>"
                                dialogs:*file-name*)
                        'do-save-file-confirmed?)
        (do-save-file-confirmed? t))))

(defun do-save-file-confirmed? (save)
  (when save
    (setf *file* dialogs:*file-name*)
    (save-to-file *file*)))

(defun save-changes (&optional confirm)
  (when (and *file*
             (q< |canUndo| ui:*edit*)
             (if confirm
                 (confirm-dialog "Save changes?"
                                 (format nil "Save changes to current file?<br><br>~S<br>"
                                         *file*)
                                 'save-changes-confirmed?)
                 t))
    (save-changes-confirmed? t)))

(defun save-changes-confirmed? (save)
  (when save
    (save-to-file *file*)))

;;; select all, cut, copy, paste

(defvar *selected-text*      "")
(defvar *selection-start*    0)
(defvar *cursor-indent-copy* 0)

(defun copy-paste (pos) ; called from QML
  (select-expression pos)
  (q! |open| ui:*clipboard-menu*))

(defun select-expression (&optional cursor-position)
  "Selects whole s-expr, if one is found; otherwise just selects current line."
  (let* ((edit (active-edit))
         (text (q< |text| edit))
         (pos (or cursor-position (q< |cursorPosition| edit)))
         (start (max 0 (1- pos)))
         ch)
    (flet ((select (start end)
             (setf *selection-start* start
                   *selected-text*   (subseq text start end))
             (q! |select| edit start end)))
      (when (< pos (length text))
        (x:while (char/= #\( (setf ch (char text start)))
          (when (or (minusp (decf start))
                    (find ch '(#\Newline #\) )))
            (let ((nl-l (position #\Newline text :end pos :from-end t))
                  (nl-r (position #\Newline text :start pos)))
              (select (if nl-l (1+ nl-l) 0)
                      (or nl-r (length text)))
              (return-from select-expression))))
        (when (and (plusp start)
                   (char= #\` (char text (1- start))))
          (decf start))
        (x:when-it (end-position (subseq text start))
          (select start
                  (+ start x:it)))))))

(defun select-all ()
  (setf *selection-start* nil)
  (q! |selectAll| (active-edit)))

(defun cut ()
  (copy)
  (q! |remove| (active-edit)
      *selection-start*
      (+ *selection-start* (length (clipboard-text)))))

(defun copy ()
  (let ((edit (active-edit)))
    (if *selection-start*
        (progn
          (set-clipboard-text *selected-text*)
          (let* ((snip (q! |getText| edit (max 0 (- *selection-start* 100)) *selection-start*))
                 (nl (position #\Newline snip :from-end t)))
            (setf *cursor-indent-copy* (if nl (- (length snip) (1+ nl)) 0))))
        (progn
          (set-clipboard-text (q< |text| edit))
          (setf *selection-start*    0
                *cursor-indent-copy* 0)))))

(defvar *pasting* nil)

(defun paste ()
  "Paste text adapting the indentation."
  (let ((*pasting* t)
        (edit (active-edit))
        (clip-text (clipboard-text)))
    (when (and (string= ui:*command* edit)
               (find #\Newline clip-text))
      (return-from paste))
    (unless (x:empty-string clip-text)
      (let* ((lines (x:split clip-text #\Newline))
             (diff (- *cursor-indent* *cursor-indent-copy*))
             (text (with-output-to-string (s)
                     (write-line (first lines) s)
                     (dolist (line (rest lines))
                       (when (plusp diff)
                         (write-string (make-string diff) s))
                       (write-line (subseq line (if (minusp diff) (- diff) 0)) s)))))
        (q! |remove| edit
            (q< |selectionStart| edit)
            (q< |selectionEnd| edit))
        (q! |insert| edit
            (q< |cursorPosition| edit)
            (subseq text 0 (1- (length text))))))))

(defun lispify (name)
  (with-output-to-string (s)
    (x:do-string (ch name)
      (cond ((upper-case-p ch)
             (format s "-~C" ch))
            ((char= #\_ ch)
             (write-char #\- s))
            (t
             (write-char (char-upcase ch) s))))))

(defun button-pressed () ; called from QML
  (flet ((hide ()
           ;; QLATER for key release event
           (qlater (lambda () (q! |close| ui:*clipboard-menu*))))
         (timer ()
           (q! |restart| ui:*menu-timer*)))
    (let ((button (intern (lispify (q< |objectName| *caller*))
                          :keyword)))
      (case button
        (:select-all      (select-all))
        (:cut             (cut)                    (hide))
        (:copy            (copy)                   (hide))
        (:paste           (paste)                  (hide))
        (:eval-exp        (eval-single-expression) (hide))
        (:undo            (q! |undo| ui:*edit*)    (timer))
        (:redo            (q! |redo| ui:*edit*)    (timer))
        (:font-bigger     (change-font :bigger)    (timer))
        (:font-smaller    (change-font :smaller)   (timer))
        (:clear           (clear)                  (timer))
        (:open-file       (open-file)              (timer))
        (:save-file       (save-file)              (timer))
        (:eval            (eval-expression*)       (timer))
        (:history-back    (history-move "back"))
        (:history-forward (history-move "forward"))
        ((:up :down :left :right)
         (arrow-pressed button)))))
  (values)) ; no return value to QML

(defun button-pressed-and-helt () ; called from QML
  (let ((button (intern (lispify (q< |objectName| *caller*))
                        :keyword)))
    (case button
      ((:up :down :left :right)
       (arrow-helt button))))
  (values)) ; no return value to QML

#+ios
(defun key-pressed (key object-name) ; called form Qt extension
  ;; hack for iOS external keyboard
  (cond ((string= "Tab" key)
         (let ((window (intern (string-upcase object-name) :keyword)))
           (when (find window '(:edit :command))
             (q! |forceActiveFocus| (if (eql :edit window)
                                    ui:*command*
                                    ui:*edit*)))))
        ((string= "Alt+E" key)
         (select-expression))
        ((string= "Alt+L" key)
         (eval-single-expression)))
  (values)) ; no return value to Qt extension

;;; ini

(let ((curr 0)
      (all 2))
  (defun set-text-document (name document) ; called from QML
    (setf (symbol-value (cond ((string= ui:*edit* name)
                               '*qml-document-edit*)
                              ((string= ui:*command* name)
                               '*qml-document-command*)))
          (qt:text-document qt:*cpp* document))
    (when (= all (incf curr))
      (ini-highlighters)
      (qt:connect-document-changed qt:*cpp* *qml-document-edit*    "edit")
      (qt:connect-document-changed qt:*cpp* *qml-document-command* "command")))
  (defun reset-documents ()
    (setf curr 0)))

(defun delayed-ini () ; called from QML
  (qlater 'apply-colors)
  (let ((timeout 1000))
    (when (qjs |isLandscape| ui:*main*)
      (qsingle-shot timeout (lambda () (q! |forceActiveFocus| ui:*edit*)))
      (incf timeout 500))
    (qsingle-shot timeout (lambda () (q! |forceActiveFocus| ui:*command*)))))

(defun orientation-changed (orientation) ; called from QML
  ;; avoid possible resize problem (virtual keyboard and landscape orientation)
  (when (/= 1 orientation) ; 1 = portrait
    (q> |x| ui:*buttons-right* (- (q< |width| ui:*buttons-right*))) ; hide
    (when (qjs |keyboardVisible| ui:*main*)
      (qsingle-shot 250 (lambda () (q! |forceActiveFocus| ui:*edit*))))))

;;; cursor movement (see arrow buttons in QML)

(defvar *focus-editor* ui:*command*)

(defun set-focus-editor (qml-name) ; called from QML
  (setf *focus-editor* qml-name))

(defun ensure-focus (&optional show)
  (q! |forceActiveFocus| *focus-editor*)
  (qjs |showKeyboard| ui:*main* show))

(defun arrow-pressed (direction)
  (let ((new-pos (if (find direction '(:left :right))
                     (+ (q< |cursorPosition| *focus-editor*)
                        (if (eql :right direction) 1 -1))
                     (let ((rect (q< |cursorRectangle| *focus-editor*)))
                       (q! |positionAt| *focus-editor*
                           (truncate (first rect))
                           (truncate (+ (second rect)
                                        (if (eql :down direction)
                                            (1+ (fourth rect))
                                            -1))))))))
    (q> |cursorPosition| *focus-editor* 
        (max 0 (min new-pos (q< |length| *focus-editor*))))))

(defun arrow-helt (direction)
  (let ((rect (q< |cursorRectangle| *focus-editor*)))
    (q> |cursorPosition| *focus-editor*
        (q! |positionAt| *focus-editor*
            (case direction
              ((:left :up)
               0)
              (t
               (q< |paintedWidth| *focus-editor*)))
            (case direction
              ((:left :right)
               (1+ (second rect)))
              (:up
               0)
              (:down
               (q< |paintedHeight| *focus-editor*)))))))

;;; ini

(defun ini ()
  #+ios
  (disable-clipboard-menu)
  (qt:ini)
  (when (q< |small| ui:*main*)
    (change-font :smaller 3))
  (eval:ini :output       'print-eval-output
            :query-dialog 'dialogs:query-dialog
            :debug-dialog 'dialogs:debug-dialog)
  #+ios
  (qt:connect-key-pressed qt:*cpp*) ; for iOS external keyboard
  (append-output (format nil "~% :h for help") :bold t))

;;; quit app

(defun back-pressed () ; called from QML
  (or (dialogs:pop-dialog)
      (progn
        (save-changes)
        (qquit))))

(qlater 'ini)
