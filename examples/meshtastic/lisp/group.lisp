(in-package :group)

(defun add-person (person)
  "Adds passed PERSON (a PLIST) to QML item model.
  The model keys are:
  :radio-name :custom-name :node-num :unread :current"
  (qjs |addPerson| ui:*group* person))

(defun clear ()
  (q! |clear| ui:*group*))

(defun radio-names ()
  (qjs |radioNames| ui:*group*))

(defun name-edited (radio name) ; called from QML
  (app:change-setting radio name :sub-key :custom-name)
  (values))

(defun set-unread-state (state)
  (q> |visible| ui:*unread-messages* state))

(defun set-unread (name n)
  (qjs |setUnread| ui:*group*
       name n)
  (when (plusp n)
    (set-unread-state t)))

(defun receiver-changed ()
  (let ((curr-name (app:setting :latest-receiver)))
    (app:change-setting curr-name 0 :sub-key :unread-messages)
    (set-unread curr-name 0)
    (dolist (name (radio-names))
      (x:when-it (app:setting name :unread-messages)
        (unless (zerop x:it)
          (return-from receiver-changed))))
    (set-unread-state nil)))
