(in-package :app)

(defun ini ()
  (qt:ini)
  (load-settings)
  (lora:ini)
  (db:ini)
  (loc:ini)
  (setf msg:*message-id* (db:max-message-id))
  (if (setting :latest-receiver)
      (msg:show-messages)
      (q> |currentIndex| ui:*main-view* 0)) ; 'Group'
  (q> |playing| ui:*loading* nil)
  (q> |interactive| ui:*main-view* t)
  #+android
  (progn
    (ensure-permissions :access-fine-location) ; for sharing location
    (ensure-permissions :bluetooth-scan :bluetooth-connect)) ; android >= 12
  (lora:start-device-discovery (or (setting :device) "")))

(defun view-index-changed (index) ; see QML
  (when (and (= 1 index)
             (not (app:setting :latest-receiver)))
    (q> |currentIndex| ui:*main-view* 0))
  (q> |visible| ui:*location* (= 0 index))
  (q> |visible| ui:*find*     (= 1 index))
  (values))

(defun icon-press-and-hold (name) ; see QML
  (cond ((string= ui:*radio-icon* name)
         ;; force update devices
         (lora:start-device-discovery (or (setting :device) "")))
        ((string= ui:*group-icon* name)
         ;; force update nodes
         (lora:start-config)))
  (values))

;;; settings

(defvar *file* (merge-pathnames "data/settings.exp"))

(defun load-settings ()
  (when (probe-file *file*)
    (with-open-file (s *file*)
      (setf lora:*settings* (read s)))))

(defun save-settings ()
  (with-open-file (s *file* :direction :output :if-exists :supersede)
    (let ((*print-pretty* nil))
      (prin1 lora:*settings* s))))

(defun kw (string)
  "Intern in KEYWORD package."
  (intern (string-upcase string) :keyword))

(defun setting (key &optional sub-key)
  (when (stringp key)
    (setf key (kw key)))
  (let ((value (getf lora:*settings* key)))
    (if sub-key
        (getf value sub-key)
        value)))

(defun change-setting (key value &key cons sub-key)
  (when (stringp key)
    (setf key (kw key)))
  (setf (getf lora:*settings* key)
        (cond (cons
               (cons value (setting key)))
              (sub-key
               (let ((plist (getf lora:*settings* key)))
                 (setf (getf plist sub-key) value)
                 plist))
              (t
               value)))
  (save-settings))

(defun my-ip ()
  (let ((ip (qrun* (qt:local-ip qt:*cpp*)))) ; 'qrun*' for return value
    (or ip "127.0.0.1")))

;;; toast

(defun toast (message &optional (seconds 3))
  (qjs |message| ui:*toast* message seconds))

(qlater 'ini)
