(in-package :db)

(defvar *file* (merge-pathnames "data/db"))

(defun query (query &rest values)
  (qt:sql-query qt:*cpp* query values))

(defun ini ()
  (ensure-directories-exist *file*)
  (qt:ini-db qt:*cpp* (namestring *file*))
  (query "create table if not exists messages (mid integer primary key, uid integer, message text)"))

(defun save-message (mid uid message)
  (query "insert into messages (mid, uid, message) values (?, ?, ?)"
         mid uid message))

(defun load-message (mid)
  (first (query "select message from messages where mid = ?"
                mid)))

(defun update-message (mid message)
  (query "update messages set message = ? where mid = ?"
         message mid))

(defun load-messages (uid)
  (query "select message from messages where uid = ? order by mid"
         uid))

(defun max-message-id ()
  (let ((val (first (query "select max(mid) from messages"))))
    (if (numberp val) val 0)))
