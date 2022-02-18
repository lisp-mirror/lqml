;;; copies all template files to new directory passed as app name

(defvar *name* (first (last (ext:command-args))))

(when (search "copy-template" *name*)
  (print "Usage: ecl -shell copy-template.lisp <name>.")
  (terpri)
  (ext:quit))

(defun string-substitute (new old string)
  (let ((len (length old)))
    (with-output-to-string (s)
      (do ((e (search old string) (search old string :start2 (+ e len)))
           (b 0 (+ e len)))
          ((not e) (write-string (subseq string b) s))
        (write-string (subseq string b e) s)
        (write-string new s)))))

(defun copy-stream (from to)
  (let ((buf (make-array 8192 :element-type (stream-element-type from))))
    (loop
      (let ((pos (read-sequence buf from)))
        (when (zerop pos)
          (return))
        (write-sequence buf to :end pos))))
  (values))

(defun copy-file (from to)
  (let ((element-type '(unsigned-byte 8)))
    (with-open-file (in from :element-type element-type)
      (with-open-file (out to :element-type element-type
                           :direction :output :if-exists :supersede)
        (copy-stream in out)
        (finish-output out)
        (= (file-length in)
           (file-length out))))))


(dolist (file (directory "app-template/**/*.*"))
  (let* ((from (namestring file))
         (to (string-substitute (format nil "/~A/" *name*)
                                "/app-template/"
                                from)))
    (if (probe-file to)
        (format t "~&skipping exisitng file: ~A" to)
        (progn
          (format t "~&copying file: ~A" to)
          (ensure-directories-exist to)
          (unless (copy-file from to)
            (error "File ~A could not be copied." to))))))

(terpri)
