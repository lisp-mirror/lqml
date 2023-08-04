(in-package :msg)

(defvar *message-id* 0)
(defvar *states*     '(:not-received :sending :received))

(defun show-message-p (message)
  (let ((user (app:setting :latest-receiver)))
    (and user (or (string= user (getf message :receiver))
                  (string= user (getf message :sender))))))

(defun add-message (message &optional loading)
  "Adds passed MESSAGE (a PLIST) to the QML item model and saves it to the DB.
  The model keys are:
  :receiver :sender :sender-name :timestamp :hour :text :mid :ack-state :me
  :hidden"
  (unless (or loading (getf message :me))
    (x:when-it (app:setting (getf message :sender) :custom-name)
      (setf (getf message :sender-name) x:it)))
  (unless loading
    (db:save-message (parse-integer (getf message :mid))
                     (parse-integer (getf message (if (getf message :me) :receiver :sender))
                                    :radix 16)
                     (prin1-to-string message)))
  (if (or loading (show-message-p message))
      (qjs |addMessage| ui:*messages* message)
      (let* ((sender (getf message :sender))
             (unread (1+ (or (app:setting sender :unread-messages) 0))))
        (app:change-setting sender unread :sub-key :unread-messages)
        (group:set-unread sender unread)))
  (unless loading
    (q! |positionViewAtEnd| ui:*message-view*)))

(defun change-state (state mid)
  (let* ((i-state (position state *states*))
         (mid* (parse-integer mid))
         (message (db:load-message mid*)))
    (when message
      (setf message (read-from-string message))
      (setf (getf message :ack-state) i-state)
      (db:update-message mid* (prin1-to-string message))
      (qjs |changeState| ui:*messages*
           i-state mid))))

(defun show-messages ()
  (x:when-it (app:setting :latest-receiver)
    (q! |clear| ui:*messages*)
    (dolist (message (db:load-messages (parse-integer x:it :radix 16)))
      (add-message (read-from-string message) t))
    (dotimes (n 2) ; Qt bug
      (q! |positionViewAtEnd| ui:*message-view*))))

(defun receiver-changed ()
  (qsleep 0.1)
  (q> |currentIndex| ui:*main-view* 1) ; 'Messages'
  (show-messages))

(defun check-utf8-length (&optional (text (q< |text| ui:*edit*))) ; see QML
  "Checks the actual number of bytes to send (e.g. an emoji is 4 utf8 bytes),
  because we can't exceed 234 bytes, which will give 312 bytes encoded protobuf
  payload."
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
        (my-pos #+mobile (nbutlast (loc:last-gps-position) 1)
                #-mobile nil))
    (format nil "~A~%~%snr: <b>~F</b> rssi: <b>~D</b>~%lat: ~,5F lon: ~,5F~%distance: <b>~:D m</b>"
                text snr rssi
                (if my-pos (first my-pos)  "-")
                (if my-pos (second my-pos) "-")
                (if (and from-pos my-pos)
                    (loc:distance my-pos from-pos)
                    "-"))))

