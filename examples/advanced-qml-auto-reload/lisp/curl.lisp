(in-package :qml)

(defun curl (url)
  "args: (url)
  Trivial download of UTF-8 encoded files, or binary files."
  (multiple-value-bind (response headers stream)
      (loop
         (multiple-value-bind (response headers stream)
             (ecl-curl::url-connection url)
           (unless (member response '(301 302))
             (return (values response headers stream)))
           (close stream)
           (setf url (header-value :location headers))))
    (if (>= response 400)
        (qlog "curl download error:" :url url :response response)
        (let ((byte-array (make-array 0 :adjustable t :fill-pointer t
                                      :element-type '(unsigned-byte 8)))
              (type (pathname-type url)))
          (x:while-it (read-byte stream nil nil)
            (vector-push-extend x:it byte-array))
          (close stream)
          (if (or (search type "txt html lisp")
                  (search "/cgi-bin/" (namestring url)))
              (qfrom-utf8 byte-array)
              byte-array)))))

(export 'curl)

