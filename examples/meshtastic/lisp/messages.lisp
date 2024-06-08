(in-package :msg)

(defvar *states*     '(:not-received :sending :received))
(defvar *message-id* 0)

(defun ini ()
  (q> |fontSize| ui:*message-view*
      (or (app:setting :message-font-size) 18)))

(defun new-message-id ()
  (mod (incf *message-id*) #.(expt 2 32)))

(let (message-ids)
  ;; associate (temporary) mesh message id to (unique) DB 'mid'
  (defun add-message-id (ids)
    (push ids message-ids))
  (defun db-mid (id)
    (cdr (assoc id message-ids))))

(defun show-message-p (message)
  (let ((user (app:setting :latest-receiver)))
    (and (not app:*background-mode*)
         user
         (or (string= user (getf message :receiver))
             (string= user (getf message :sender))))))

(defun add-message (message &optional loading)
  "Adds passed MESSAGE (a PLIST) to the QML item model and saves it to the DB.
  The model keys are:
  :receiver :sender :sender-name :timestamp :hour :text :mid :ack-state :me
  :hidden"
  (x:when-it (getf message (if (getf message :me) :receiver :sender))
    (unless (or loading (getf message :me))
      (let ((name (app:setting (getf message :sender) :custom-name)))
        (when (and name (string/= "~" name))
          (setf (getf message :sender-name) name))))
    (unless loading
      (let ((id (getf message :mid)))
        (remf message :mid)
        (let ((db-mid (db:save-message x:it (prin1-to-string message))))
          (add-message-id (cons id db-mid))
          (setf (getf message :mid) db-mid))))
    (if (or loading (show-message-p message))
        (qjs |addMessage| ui:*messages* message)
        (let* ((sender (getf message :sender))
               (unread (1+ (or (app:setting sender :unread-messages) 0))))
          (unless (x:starts-with "<b>:e" (getf message :text)) ; 'echo' message
            (app:change-setting sender unread :sub-key :unread-messages)
            (group:set-unread sender unread))))
    (unless loading
      (q! |positionViewAtEnd| ui:*message-view*))))

(defun change-state (state id)
  (let* ((i-state (position state *states*))
         (db-mid (db-mid id))
         (message (db:load-message db-mid)))
    (when message
      (setf message (read-from-string message))
      (setf (getf message :ack-state) i-state)
      (db:update-message db-mid (prin1-to-string message))
      (qjs |changeState| ui:*messages*
           i-state db-mid))))

(defun show-messages ()
  (x:when-it (app:setting :latest-receiver)
    (q! |clear| ui:*messages*)
    (dolist (row (db:load-messages x:it))
      (destructuring-bind (mid message) row
        (setf message (read-from-string message)
              (getf message :mid) mid)
        (add-message message t)))
    (dotimes (n 2) ; Qt bug
      (q! |positionViewAtEnd| ui:*message-view*))))

(defun receiver-changed ()
  (qsleep 0.1)
  (q> |currentIndex| ui:*main-view* 1) ; 'Messages'
  (show-messages))

(defun check-utf8-length (&optional (text (q< |text| ui:*edit*))) ; see QML
  "Checks the actual number of bytes to send (e.g. a typical emoji is 4 utf8
  bytes), because we can't exceed 234 bytes."
  (let ((len (length (qto-utf8 text)))
        (too-long (q< |tooLong| ui:*edit*)))
    (cond ((and (not too-long)
                (> len 234))
           (q> |tooLong| ui:*edit* t))
          ((and too-long
                (<= len 234))
           (q> |tooLong| ui:*edit* nil))))
  (values))

(defun message-press-and-hold (text) ; see QML
  (set-clipboard-text text)
  (app:toast (tr "message copied") 2)
  (values))

(defun show-date (timestamp) ; see QML
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time timestamp)
    (app:toast (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                       year month day hour min sec))))

(defun find-clicked ()
  (let ((show (not (q< |visible| ui:*find-text*))))
    (q> |visible| ui:*find-text* show)
    (if show
        (q! |forceActiveFocus| ui:*find-text*)
        (progn
          (q! |clear| ui:*find-text*)
          (clear-find)))))

(defun find-text (text)
  (unless (x:empty-string text)
    (qjs |clearFind| ui:*messages*)
    (qjs |find| ui:*messages* text)))

(defun clear-find ()
  (qjs |clearFind| ui:*messages*)
  (qlater (lambda () (qjs |positionViewAtEnd| ui:*message-view*))))

(defun highlight-term (text term) ; see QML
  "Highlights TERM in red, returns NIL if TERM is not found."
  (let ((len (length term))
        found)
    (with-output-to-string (s)
      (do ((e (search term text :test 'string-equal)
              (search term text :test 'string-equal :start2 (+ e len)))
           (b 0 (+ e len)))
          ((not e) (if found
                       (write-string (subseq text b) s)
                       (return-from highlight-term)))
        (setf found t)
        (write-string (subseq text b e) s)
        (format s "<font color='red'>~A</font>"
                (subseq text e (+ e len)))))))

(defun echo-message (text from snr rssi)
  "Meant for radio signal testing: one static node (with GPS module), while
  another mobile node is moving to different places (using GPS of phone),
  sending an ':e ...' text message, which will be echoed with info about signal
  strength, position and distance."
  (let ((from-pos (loc:position* from))
        (my-pos #+mobile (nbutlast (loc:latest-gps-position) 1)
                #-mobile nil))
    (x:cc (format nil "~A~%~%snr: <b>~F</b> rssi: <b>~D</b>"
                  text snr rssi)
          (if my-pos
              (format nil "~%lat: ~,5F lon: ~,5F"
                      (first my-pos)
                      (second my-pos))
              "")
          (if (and my-pos from-pos)
              (format nil "~%distance: <b>~:D m</b>"
                      (loc:distance my-pos from-pos))
              ""))))

(defun swipe-to-left () ; see QML
  (q> |currentIndex| ui:*main-view* 0)
  (values))

(defun font-size-dialog ()
  (app:input-dialog
   (tr "Message font size:") 'font-size-changed
   :from 10.0
   :to   48.0
   :value (float (or (app:setting :message-font-size)
                     18.0))))

(defun font-size-changed (ok) ; see QML
  (when ok
    (let ((size (q< |value| ui:*dialog-spin-box*)))
      (setf size (min 48 (max 10 size)))
      (q> |fontSize| ui:*message-view* size)
      (app:change-setting :message-font-size size)))
  (values))

