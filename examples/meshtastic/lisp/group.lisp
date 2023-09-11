(in-package :group)

(defun ini ()
  (q> |model| ui:*modem*
      (mapcar (lambda (kw) (string-downcase (symbol-name kw)))
              (lora:keywords :modem-preset)))
  (x:when-it (app:setting :modem-preset)
    (q> |currentIndex| ui:*modem*
        (q! |indexOfValue| ui:*modem*
            (string-downcase (symbol-name x:it))))))

(defun add-person (person)
  "Adds passed PERSON (a PLIST) to QML item model.
  The model keys are:
  :radio-name :custom-name :node-num :unread :current"
  (when (zerop (q< |count| ui:*group*))
    ;; special item 'Broadcast'
    (qjs |addPerson| ui:*group*
         (list :radio-name lora:*broadcast-name*
               :custom-name (tr "Broadcast")
               :node-num lora:+broadcast-id+
               :current (equal (app:setting :latest-receiver)
                               lora:*broadcast-name*))))
  (qjs |addPerson| ui:*group* person))

(defun clear ()
  (setf lora:*schedule-clear* nil)
  (q! |clear| ui:*group*))

(defun radio-names ()
  (qjs |radioNames| ui:*group*))

(defun name-edited (radio name) ; see QML
  (app:change-setting radio (if (string= (tr "Anonym") name) "" name)
                      :sub-key :custom-name)
  (values))

(defun set-unread-state (state)
  (q> |visible| ui:*unread-messages* state))

(defun set-unread (name n)
  (unless (string= lora:*broadcast-name* name)
    (qjs |setUnread| ui:*group*
         name n)
    (when (plusp n)
      (set-unread-state t))))

(defun receiver-changed ()
  (let ((curr-name (app:setting :latest-receiver)))
    (unless (string= lora:*broadcast-name* curr-name)
      (app:change-setting curr-name 0 :sub-key :unread-messages)
      (set-unread curr-name 0)
      (dolist (name (radio-names))
        (x:when-it (app:setting name :unread-messages)
          (unless (zerop x:it)
            (return-from receiver-changed))))
      (set-unread-state nil))))

