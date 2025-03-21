(in-package :app)

(defun ini ()
  (qt:ini)
  (db:ini)
  (populate-db)
  ;; loads image directly from database, see 'cpp/qt.cpp::DatabaseImageProvider'
  (q> |source| ui:*logo* "image://db/logo-128"))

(defun in-data-path (&optional (file "") (prefix "data/"))
  #+mobile
  (merge-pathnames (x:cc prefix file))
  #-mobile
  (x:cc (qrun* (qt:data-path qt:*cpp* prefix)) file))

(defun file-bytes (file)
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (let ((arr (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence arr s)
      arr)))

;; put images in database

(defun populate-db ()
  (let ((files (sort (directory (merge-pathnames "blob/*.png"))
                     'string< :key 'pathname-name)))
    (when (/= (length files) (db:size))
      (if (probe-file (merge-pathnames "blob/"))
          (progn
            (db:delete-all-images)
            (dolist (file files)
              (db:save-image (pathname-name file) (file-bytes file)))))
          (x:d "No 'blob/' directory found."))))

(qlater 'ini)
