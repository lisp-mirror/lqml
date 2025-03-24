(in-package :db)

(defvar *file* nil)

(defun query (query &rest values)
  (qrun* (qt:sql-query qt:*cpp* query values)))

(defun ini ()
  (setf *file* (app:in-data-path "db"))
  (ensure-directories-exist *file*)
  (qt:ini-db qt:*cpp* (namestring *file*) *quick-view*)
  (query "create table if not exists images (id integer primary key autoincrement, name text, data blob)"))

(defun size ()
  (first (query "select count(*) from images")))

(defun save-image (name data)
  "Inserts image NAME, DATA (vector of octets) and returns the new image ID."
  (first (query "insert into images (name, data) values (?, ?)"
                name data)))

(defun load-images ()
  (query "select name, data from images order by id"))

(defun delete-image (id)
  (query "delete from images where id = ?"
         id)
  (values))

(defun delete-all-images ()
  (query "delete from images"))

