(in-package :msg)

(defvar *message-id* 0)
(defvar *states*     '(:not-received :sending :received))

(defun show-message-p (message)
  (let ((user (app:setting :latest-receiver)))
    (and user (or (string= user (getf message :receiver))
                  (string= user (getf message :sender))))))

(defun add-message (message &optional loading)
  "Adds passed MESSAGE (a PLIST) to both the QML item model and *MESSAGES*.
  The model keys are:
  :receiver :sender :sender-name :timestamp :hour :text :mid :ack-state :me"
  (unless (or loading (getf message :me))
    (x:when-it (app:setting (getf message :sender) :custom-name)
      (setf (getf message :sender-name) x:it)))
  (unless loading
    (db:save-message (getf message :mid)
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
    (dolist (row (db:load-messages (parse-integer x:it :radix 16)))
      (add-message (read-from-string (first row)) t))
    (q! |positionViewAtEnd| ui:*message-view*)))

(defun receiver-changed ()
  (qsleep 0.1)
  (q> |currentIndex| ui:*main-view* 1) ; 'Messages'
  (show-messages))
