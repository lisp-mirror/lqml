(in-package :db)

(defvar *file* nil)

(defun query (query &rest values)
  (let ((rows (and (x:starts-with "select" query)
                   (1+ (count #\, (subseq query 0 (search "from" query)))))))
    (qrun* (qt:sql-query qt:*cpp* query values rows))))

(defun ini ()
  (setf *file* (app:in-data-path "db"))
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

(defun delete-message (mid) ; see QML
  (query "delete from messages where mid = ?"
         mid)
  (values))

(defun export-to-list () ; see QML
  "Exports db to a LIST and saves it as 'db.exp'."
  (with-standard-io-syntax
    (with-open-file (s (app:in-data-path "db.exp")
                       :direction :output :if-exists :supersede)
      (write-char #\( s)
      (let ((rows (query "select mid, uid, message from messages order by mid")))
        (dolist (row rows)
          (let* ((plist (read-from-string (third row)))
                 ;; Lisp and Qt utf-8 differ
                 (text (babel:octets-to-string
                        (qto-utf8 (getf plist :text)))))
            (setf (getf plist :text) text
                  (third row)        plist))
          (print row s)))
      (terpri s)
      (write-char #\) s)))
  (values))
