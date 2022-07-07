;;; usage: ecl -shell kml.lisp

(in-package :cl-user)

(defun file-contents (file)
  (with-open-file (s file)
    (let ((str (make-string (file-length s))))
      (read-sequence str s)
      str)))

(defvar *kml-1* (file-contents "template-1.kml"))
(defvar *kml-2* (file-contents "template-2.kml"))
(defvar *kml-3* (file-contents "template-3.kml"))
(defvar *kml-4* (file-contents "template-4.kml"))
(defvar *kml-5* (file-contents "template-5.kml"))

(defun cc (&rest strings)
  (apply 'concatenate 'string strings))

(defun split (str &optional (sep #\,))
  (unless (zerop (length str))
    (let (list)
      (do ((e (position sep str) (position sep str :start (1+ e)))
           (b 0 (1+ e)))
          ((not e) (push (subseq str b) list))
        (push (subseq str b e) list))
      (nreverse list))))

(defun generate-kml (file)
  (let* ((name (cc "logs/" file "."))
         (csv (cc name "csv"))
         (kml (cc name "kml"))
         (kmz (cc name "kmz")))
    (unless (probe-file kmz)
      (format t "~&generating ~S~%" kmz)
      (let* ((point "point.png")
             (header "<h2>~A</h2>~%")
             (row    "<tr><td class=s~D>~A</td><td>~A</td></tr>~%")
             (lines (with-open-file (s csv)
                      (loop :for line = (split (read-line s nil nil))
                        :while line :collect line)))
             (i-timestamp 0)
             (i-accuracy  3)
             (i-direction 5)
             (i-lat       6)
             (i-lon       7)
             (i-speed     8)
             (i-distance  9)
             (*print-pretty* nil))
        (setf lines (rest lines)) ; cut header
        (with-open-file (s kml :direction :output :if-exists :supersede)
          (write-string *kml-1* s)
          (dolist (line lines)
            (format s "~A,~A,0 "
                    (nth i-lon line)
                    (nth i-lat line)))
          (write-string *kml-2* s)
          (let ((c 0))
            (dolist (line lines)
              (when (zerop (mod (incf c) 20)) ; a marker every 20 seconds
                (write-string *kml-3* s)
                (format s header (nth i-timestamp line))
                (write-line "<table>" s)
                ;; table of log values
                (format s row 2
                        (nth i-distance line)
                        "m")
                (format s row 2
                        (nth i-speed line)
                        "km/h")
                (format s row 1
                        (nth i-accuracy line)
                        "acc")
                (format s row 1
                        (nth i-direction line)
                        "°")
                ;; position
                (format s *kml-4*
                        (nth i-lon line)
                        (nth i-lat line)))))
          (write-string *kml-5* s))
        (ext:run-program "zip" (list kmz kml point))
        (delete-file kml)))))

(dolist (file (directory "logs/*.csv"))
  (generate-kml (pathname-name file)))

