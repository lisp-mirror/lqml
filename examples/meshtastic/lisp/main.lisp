(in-package :app)

(defun ini ()
  (qt:ini)
  (restore-eventual-backup)
  (load-settings)
  (lora:ini)
  (group:ini)
  (msg:ini)
  (radios:ini)
  (db:ini)
  (loc:ini)
  (if (setting :latest-receiver)
      (msg:show-messages)
      (qlater (lambda () (q> |currentIndex| ui:*main-view* 0)))) ; 'Group'
  (x:when-it (setting :recent-emojis)
    (setf *recent-emojis* (mapcar 'qfrom-utf8 x:it))
    (q> |model| ui:*recent-emojis* *recent-emojis*))
  (q> |running| ui:*hourglass* nil)
  (q> |visible| ui:*message-view* t)
  #+android
  (progn
    (ensure-permissions :access-fine-location) ; for sharing location
    (ensure-permissions :bluetooth-scan :bluetooth-connect)) ; android >= 12
  #+(or android ios)
  (qlater (lambda () (qt:keep-screen-on qt:*cpp*)))
  (lora:start-device-discovery (or (setting :device) "")))

(defun in-data-path (&optional (file "") (prefix "data/"))
  #+mobile
  (merge-pathnames (x:cc prefix file))
  #-mobile
  (x:cc (qrun* (qt:data-path qt:*cpp* prefix)) file))

(defun view-index-changed (index) ; see QML
  (when (and (= 1 index)
             (not (app:setting :latest-receiver)))
    (q> |currentIndex| ui:*main-view* 0))
  (q> |visible| ui:*location* (= 0 index))
  (q> |visible| ui:*find*     (= 1 index))
  (q> |interactive| ui:*main-view* (/= 1 index)) ; swipe single message, not view
  (values))

(defun icon-press-and-hold (name) ; see QML
  (cond ((string= ui:*radio-icon* name)
         ;; force update of: device discovery
         (lora:start-device-discovery (or (setting :device) "")))
        ((string= ui:*group-icon* name)
         ;; force update of: node configuration
         (lora:get-node-config)))
  (values))

;;; settings

(let (file)
  (defun load-settings ()
    (unless file
      (setf file (in-data-path "settings.exp")))
    (when (probe-file file)
      (with-open-file (s file)
        (setf lora:*settings* (read s)))))
  (defun save-settings ()
    (with-open-file (s file :direction :output :if-exists :supersede)
      (let ((*print-pretty* nil))
        (prin1 lora:*settings* s)))))

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

;;; emojis (by default desktop only)

(defvar *recent-emojis* (q< |model| ui:*recent-emojis*))

(defun emoji-clicked (emoji) ; see QML
  (q! |insert| ui:*edit*
      (q< |cursorPosition| ui:*edit*) emoji)
  (q> |visible| ui:*emojis* nil)
  (setf *recent-emojis* (delete emoji *recent-emojis* :test 'string=))
  (pushnew emoji *recent-emojis* :test 'string=)
  (when (> (length *recent-emojis*) 10)
    (setf *recent-emojis* (butlast *recent-emojis*)))
  (q> |model| ui:*recent-emojis* *recent-emojis*)
  (change-setting :recent-emojis (mapcar 'qto-utf8 *recent-emojis*))
  (values))

;;; toast

(defun toast (message &optional (seconds 3))
  "Shows a temporary message/notification. If the passed time is 0 seconds, the
  message will be shown until the user taps on the message."
  (qjs |message| ui:*toast* message (float seconds)))

;;; dialogs

(defun message-dialog (text)
  (qjs |message| ui:*dialogs* text))

(defun confirm-dialog (text callback)
  (qjs |confirm| ui:*dialogs*
       text (x:callback-name callback)))

(defun input-dialog (label callback &key (title "")
                     (text "") (max-length #.(float 32767))
                     from to value)
  (qjs |input| ui:*dialogs*
       title label (x:callback-name callback)
       text max-length ; string (line edit)
       from to value)) ; integer (spin box)

;;; backup/restore all app data

(defvar *backup-data-file* "mt-data.zip")
(defvar *backup-map-file*  "map.bin")

(defun zip (zip-file directory)
  "Creates a *.zip file of passed directory, _not_ including the directory name."
  (zip:zip zip-file directory :if-exists :supersede)
  zip-file)

(defun unzip (zip-file &optional (directory "."))
  "Extracts (previously uploaded) *.zip file."
  (zip:unzip zip-file directory :if-exists :supersede)
  zip-file)

(defun make-backup ()
  "Creates backup files in local data directory '.../cl-mestastic/backup/'."
  (ensure-directories-exist (in-data-path "" "backup/"))
  (zip (in-data-path *backup-data-file* "backup/")
       (in-data-path))
  (loc:make-map-bin)
  (toast (tr "backup ready")))

(defun restore-eventual-backup ()
  "Checks if there are backup files in local data directory
  '.../cl-mestastic/'. If found, the data is restored and the backup files are
  deleted."
  (x:when-it (probe-file (in-data-path *backup-data-file* ""))
    (unzip x:it (app:in-data-path))
    (delete-file x:it))
  (loc:extract-map-bin t))

;;; check app version (mobile)

#+mobile
(defconstant +version+ 1)

#+mobile
(let ((.version (merge-pathnames ".version")))
  (when (or (not (probe-file .version))
            (> +version+
               (parse-integer (alexandria:read-file-into-string .version))))
    ;; asset files may have changed
    (copy-all-asset-files)
    (alexandria:write-string-into-file
     (princ-to-string +version+) .version :if-exists :supersede)))

(qlater 'ini)
