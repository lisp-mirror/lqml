(in-package :db)

(defvar *file* (merge-pathnames "data/db"))

(defun ini ()
  (with-open-database (db *file*)
    (execute-non-query
     db "create table if not exists messages (mid integer primary key, uid integer, message text)")))

(defun save-message (mid uid message)
  (with-open-database (db *file*)
    (execute-non-query
     db "insert into messages (mid, uid, message) values (?, ?, ?)" mid uid message)))

(defun load-message (mid)
  (with-open-database (db *file*)
    (execute-single
     db "select message from messages where mid = ?" mid)))

(defun update-message (mid message)
  (with-open-database (db *file*)
    (execute-non-query
     db "update messages set message = ? where mid = ?" message mid)))

(defun load-messages (uid)
  (with-open-database (db *file*)
    (execute-to-list
     db "select message from messages where uid = ? order by mid" uid)))

(defun max-message-id ()
  (or (with-open-database (db *file*)
        (execute-single db "select max(mid) from messages"))
      0))

