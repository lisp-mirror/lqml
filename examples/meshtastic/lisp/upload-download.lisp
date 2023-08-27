;;; zip/unzip

(in-package :qml)

#+ios
(defun cl-user::read-sequence* (buffer stream &rest arguments)
  ;; hack for iOS, where 'read-sequence' doesn't update 'file-position';
  ;; every occurrence of 'read-sequence' in library :zip must be replaced
  ;; with this function;
  ;; we can't replace it globally, because some code (like Quicklisp)
  ;; would stop working
  (let ((p1 (file-position stream))
        (p2 (apply 'cl:read-sequence buffer stream arguments)))
    (when p1
      (file-position stream (+ p1 p2))) ; update manually
    p2))

;;; web server handler

(in-package :s-http-server)

(defvar *web-server* nil)
(defvar *empty-line* #.(map 'vector 'char-code (list #\Return #\Linefeed
                                                     #\Return #\Linefeed)))

(defconstant +buffer-length+ 8192)

(defun form-data-filename (data start end)
  "Searches for 'filename=' in current form data field header."
  (let ((p1 (search #.(x:string-to-bytes "filename=\"")
                    data :start2 start :end2 end)))
    (when p1
      (incf p1 #.(length "filename=\""))
      (x:bytes-to-string
       (subseq data p1 (position #.(char-code #\") data :start p1))))))

(defun save-file (stream content-length boundary)
  "Saves uploaded file(s) under 'data/'. Requires 'multipart/form-data'."
  ;; read all data into a buffer first
  (let* ((content (make-array 0 :element-type '(unsigned-byte 8) :adjustable t))
         (boundary-length (length boundary))
         (start (+ boundary-length 2)))
    (loop :with buffer = (make-array +buffer-length+ :element-type '(unsigned-byte 8))
          :for index = 0 :then (+ index pos)
          ;; don't read past end, would block http connection
          :for pos = (read-sequence buffer stream :end (min +buffer-length+
                                                            (- content-length index)))
          :do (adjust-array content (+ index pos))
              (replace content buffer :start1 index :end2 pos)
          :while (< index content-length))
    ;; loop through all form-data and save file(s)
    (x:while-it (search boundary content :start2 start)
      (let ((filename (form-data-filename content (+ start 2) (- x:it 2))))
        (unless (x:empty-string filename)
          (let ((pathname (merge-pathnames (x:cc (if (string= "bin" (pathname-type filename)) "" "data/")
                                                 filename))))
            (ensure-directories-exist pathname)
            (with-open-file (out pathname :direction :output :if-exists :supersede
                                          :element-type '(unsigned-byte 8))
              (write-sequence (subseq content
                                      (+ 4 (search *empty-line* content :start2 start))
                                      (- x:it 4))
                              out)))))
      (setf start (+ x:it boundary-length)))))

(defun ensure-multipart/form-data (headers)
  "Searches headers for 'multipart/form-data' and returns its boundary string."
  (let ((content-type (cdr (assoc :content-type headers))))
    (when (search "multipart/form-data" content-type)
      (x:string-to-bytes
       (subseq content-type (+ (search "boundary=" content-type)
                               #.(length "boundary=")))))))

(defun static-resource/upload-handler (s-http-server handler http-request stream)
  "Hosts static resources from a document root. If the http request is POST,
it saves uploaded files on the server."
  ;; slightely extended version of 's-http-server:static-resource-handler'
  ;; (see 'upload' below)
  (destructuring-bind (context document-root &rest options)
      handler
    (let* ((path (get-path http-request))
           (resource-pathname (compute-real-resource-pathname document-root path context
                                                              (or (getf options :pathname-builder)
                                                                  #'make-real-resource-pathname)))
           (expires-max-age (or (getf options :expires-max-age) (* 60 60))))
      (if (probe-file resource-pathname)
          (progn
            ;; upload
            (when (eql :post (get-method http-request))
              (let* ((headers (get-headers http-request))
                     (boundary (ensure-multipart/form-data headers)))
                (when boundary
                  (save-file (get-stream (get-http-connection http-request))
                             (parse-integer (cdr (assoc :content-length headers)))
                             boundary)
                  (loc:check-offline-map) ; evtl. tiles to extract
                  ;; if uploaded file is app:*backup-data-file*, unzip data, close app
                  ;; (restart needed)
                  (let ((data.zip (x:cc "data/" app:*backup-data-file*)))
                    (x:when-it (probe-file data.zip)
                      (qml:qlog "data file found, unzipping...")
                      (app:unzip x:it (app:in-data-path ""))
                      (delete-file x:it)
                      (app:toast (qml:tr "Data uploaded, closing app..."))
                      (qml:qlog "closing app...")
                      (qml:qsingle-shot 10000 'qml:qquit))))))
            (logm s-http-server :debug "Serving ~s" resource-pathname)
            (host-static-resource http-request stream resource-pathname :expires-max-age expires-max-age))
          (progn
            (logm s-http-server :error "Failed to find ~s" resource-pathname)
            (values t 404 (standard-http-html-error-response http-request stream 404 "Resource Not Found" path)))))))

(defun start ()
  (let ((ini (null *web-server*)))
    (when ini
      (setf *web-server* (make-s-http-server)))
    (start-server *web-server*)
    (when ini
      (app:make-backup)
      (qml:qlog "data zipped, ready for download")
      (register-context-handler *web-server* "/" 'static-resource/upload-handler
                                :arguments (list *default-pathname-defaults*))))
  (x:when-it (app:my-ip)
    (app:toast (format nil "http://~A:1701" x:it) 0)))

(defun stop ()
  (stop-server *web-server*)
  (setf *web-server* nil)
  (app:toast (qml:tr "web-server stopped")))

(export (list 'start 'stop))

