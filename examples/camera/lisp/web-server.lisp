;;; This starts a local web-server in order to preview/download the taken
;;; pictures on your desktop computer. Make sure you are in the same WiFi,
;;; and open:
;;;
;;; http://192.168.1.x:1701/

(in-package :camera)

(defvar *web-server* nil)
(defvar *image-path* nil)

(defvar *index.html*
"<!doctype html>
<html>
<head>
<style type=\"text/css\">
img { width: 150px; height: 150px; object-fit: contain; border-width: 10px; border-style: solid; border-color: white }
</style>
</head>
<body>
~A
</body>
</html>")

(defvar *img.htm* "<a href=~S><img src=~S /></a>")

(defun ini (image-path)
  (setf *web-server* (make-s-http-server))
  (start-server *web-server*)
  (register-context-handler *web-server* "/" 'static-resource-handler
                            :arguments (list image-path)))

(defun create-index.html (image-path rotation) ; called from QML
  "Creates 'index.html' for local web-server."
  #+ios ; on iOS the image must be rotated
  (qt:rotate-image qt:*cpp* image-path rotation)
  (unless *image-path*
    (ini image-path))
  (setf *image-path* image-path)
  (with-open-file (s (merge-pathnames "index.html" image-path)
                     :direction :output :if-exists :supersede)
    (format s *index.html*
            (x:join (mapcar (lambda (file)
                              (let ((name (x:cc (pathname-name file) ".jpg")))
                                (format nil *img.htm* name name)))
                            (sort (directory (merge-pathnames "*_0*.jpg" image-path))
                                  'string< :key 'pathname-name))
                    #\Newline)))
  (values)) ; no return value to QML

