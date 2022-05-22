(defpackage :x
  (:use :common-lisp)
  (:export
   #:callback-name
   #:cc
   #:bytes-to-string
   #:d
   #:do-string
   #:empty-string
   #:ensure-list
   #:ends-with
   #:it
   #:it*
   #:if-it
   #:if-it*
   #:join
   #:split
   #:starts-with
   #:string-substitute
   #:string-to-bytes
   #:when-it
   #:when-it*
   #:while
   #:while-it
   #:with-gensyms))

(provide :x)

(in-package :x)

(defmacro if-it (exp then &optional else)
  `(let ((it ,exp))
     (if it ,then ,else)))

(defmacro if-it* (exp then &optional else)
  `(let ((it* ,exp))
     (if it* ,then ,else)))

(defmacro when-it (exp &body body)
  `(let ((it ,exp))
     (when it ,@body)))

(defmacro when-it* (exp &body body)
  `(let ((it* ,exp))
     (when it* ,@body)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (s)
                   `(,s (gensym)))
                 syms)
     ,@body))

(defmacro while (exp &body body)
  `(do ()
       ((not ,exp))
     ,@body))

(defmacro while-it (exp &body body)
  `(do ((it))
     ((not (setf it ,exp)))
     ,@body))

(defmacro do-string ((var str) &body body)
  `(block nil
     (map nil (lambda (,var)
                ,@body)
          ,str)))

(defun d (&rest args)
  "A simple debug print."
  (print (cons :debug args)))

(defun cc (&rest args)
  "Convenient string concatenation."
  (apply 'concatenate 'string args))

(defun empty-string (s)
  (zerop (length s)))

(defun %str-with (sub str starts)
  (let ((l1 (length str))
        (l2 (length sub)))
    (when (>= l1 l2)
      (string= sub (subseq str (if starts 0 (- l1 l2)) (when starts l2))))))

(defun starts-with (sub str)
  (%str-with sub str t))

(defun ends-with (sub str)
  (%str-with sub str nil))

(defun string-substitute (new old string)
  (let ((len (length old)))
    (with-output-to-string (s)
      (do ((e (search old string) (search old string :start2 (+ e len)))
           (b 0 (+ e len)))
          ((not e) (write-string (subseq string b) s))
        (write-string (subseq string b e) s)
        (write-string new s)))))

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defun split (str &optional (sep #\Space))
  (etypecase sep
    (character
     (unless (zerop (length str))
       (let (list)
         (do ((e (position sep str) (position sep str :start (1+ e)))
              (b 0 (1+ e)))
             ((not e) (push (subseq str b) list))
           (push (subseq str b e) list))
         (nreverse list))))
    (string
     (let ((len (length sep))
           list)
       (do ((e (search sep str) (search sep str :start2 (+ e len)))
            (b 0 (+ e len)))
           ((not e) (push (subseq str b) list))
         (push (subseq str b e) list))
       (nreverse list)))))

(defun join (list &optional (sep #\Space))
  (format nil (concatenate 'string "~{~A~^" (string sep) "~}")
          list))

(defun bytes-to-string (b)
  (map 'string 'code-char b))

(defun string-to-bytes (s)
  (map 'vector 'char-code s))

(defun callback-name (callback)
  (if (stringp callback)
      callback
      (format nil "~A:~A"
              (package-name (symbol-package callback))
              (symbol-name callback))))

