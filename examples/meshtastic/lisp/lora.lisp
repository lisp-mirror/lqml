(in-package :lora)

(defvar *settings* (list :modem-preset :long-fast))

(defvar *channel-name* nil)
(defvar *my-channel*   nil)
(defvar *channels*     nil)
(defvar *my-node-info* nil)
(defvar *node-infos*   nil)
(defvar *receiver*     nil)
(defvar *config-lora*  nil)
(defvar *ble-names*    nil)
(defvar *print-json*   t)

(defun ini ()
  (setf *receiver*     (app:setting :latest-receiver)
        *channel-name* (or (app:setting :channel-name)
                           "LongFast"))
  (q> |text| ui:*channel-name* *channel-name*))

;;; header

(defun lsb (size)
  (ldb (byte 8 0) size))

(defun msb (size)
  (ldb (byte 8 8) size))

(defun header (size)
  (vector #x94 #xc3 (msb size) (lsb size)))

;;; ini/send/receive

(defconstant  +broadcast-id+   #xffffffff)
(defparameter *broadcast-name* "ffff")

(defvar *config-id*       0)
(defvar *config-complete* nil)
(defvar *notify-id*       nil)
(defvar *ready*           nil)
(defvar *reading*         nil)
(defvar *received*        nil)
(defvar *received-faulty* nil)
(defvar *schedule-clear*  t)

(defun to-bytes (list)
  (make-array (length list)
              :element-type '(unsigned-byte 8)
              :initial-contents list))

(defun start-device-discovery (&optional (name (or (app:setting :device) "")))
  (setf *schedule-clear* t)
  (setf *ble-names* nil)
  (unless radios:*found*
    (radios:clear))
  (qt:start-device-discovery qt:*cpp* name)
  (q> |playing| ui:*busy* t))

(defun get-node-config ()
  ;; see also Timer in 'qml/ext/Group.qml'
  (when *ready*
    (setf *schedule-clear* t)
    (setf *config-complete* nil
          *channels*        nil
          *node-infos*      nil)
    (incf *config-id*)
    (send-to-radio
     (me:make-to-radio :want-config-id *config-id*))
    (q> |playing| ui:*busy* t)))

(defun set-ready (&optional ready name ble-names) ; see Qt
  (setf *ready* ready)
  (when ready
    (setf *ble-names* ble-names)
    (app:toast (x:cc (tr "radio") ": " name) 2)
    (get-node-config))
  (values))

(defun add-line-breaks (text)
  (x:string-substitute "<br>" (string #\Newline) text))

(defun my-name ()
  (when *config-complete*
    (me:short-name (me:user *my-node-info*))))

(defun my-num ()
  (when *config-complete*
    (me:num *my-node-info*)))

(defun to-radio (&rest args &key from to id hop-limit want-ack priority decoded)
  (me:make-to-radio
   :packet (apply 'me:make-mesh-packet args)))

(defun send-message (text)
  "Sends TEXT to radio and adds it to QML item model."
  ;; check special keywords first
  (flet ((clear ()
           (q! |clear| ui:*edit*)))
    (cond #+mobile
          ((string= ":s" text)   ; for Lisp hackers only
           (start-swank)
           (clear))
          #+mobile
          ((string= ":w" text)   ; for saving/restoring message DB, settings, map tiles
           (s-http-server:start)
           (clear))
          #+mobile
          ((string= ":ws" text)  ; stop web-server after save/restore
           (s-http-server:stop)
           (clear))
          (t
           (msg:check-utf8-length (q< |text| ui:*edit*))
           (unless (q< |tooLong| ui:*edit*)
             (msg:new-message-id)
             (when (stringp *receiver*)
               (setf *receiver* (name-to-node *receiver*)))
             (send-to-radio
              (to-radio :from (my-num)
                        :to *receiver*
                        :id msg:*message-id*
                        :want-ack t
                        :decoded (me:make-data
                                  :portnum :text-message-app
                                  :payload (qto-utf8 text))))
             (msg:add-message
              (list :receiver (node-to-name *receiver*)
                    :sender (my-name)
                    :timestamp (get-universal-time)
                    :hour (timestamp-to-hour)
                    :text (add-line-breaks text)
                    :mid msg:*message-id*
                    :ack-state (position :sending msg:*states*)
                    :me t)))))))

(defun read-radio ()
  "Triggers a read on the radio. Will call RECEIVED-FROM-RADIO on success."
  (qt:read* qt:*cpp*))

(defun send-to-radio (to-radio)
  "Sends passed TO-RADIO, preceded by a header."
  (when *print-json*
    (pr:print-json to-radio))
  (let ((bytes (pr:serialize-to-bytes to-radio)))
    (qt:write* qt:*cpp* (header (length bytes)))
    (qt:write* qt:*cpp* bytes)))

(defun received-from-radio (bytes &optional notified) ; see Qt
  (if notified
      (progn
        (setf *notify-id* bytes)
        (read-radio))
      (multiple-value-bind (from-radio error)
          (ignore-errors (pr:deserialize-from-bytes 'me:from-radio bytes))
        (setf *reading* t)
        (if from-radio
            (progn
              (when *print-json*
                (pr:print-json from-radio))
              (push from-radio *received*))
            (progn
              (qlog "received faulty bytes: ~A" error)
              (push bytes *received-faulty*)))))
  (values))

(defun receiving-done () ; see Qt
  (setf *reading* nil)
  (process-received)
  (values))

(defun process-saved-packets (packets) ; see Qt
  "Called when app changes from background to foreground (mobile only)."
  (let ((app:*background-mode* t))
    (dolist (packet packets)
      (received-from-radio packet))
    (receiving-done))
  ;; this will show eventual red circles with numbers of unread messages
  (when (group:unread-messages-p)
    (app:change-setting :latest-receiver nil)
    (q> |currentIndex| ui:*group-view* -1)
    (q> |currentIndex| ui:*main-view* 0)) ; 'Group'
  (values))

(defun node-to-name (num)
  (if (= +broadcast-id+ num)
      *broadcast-name*
      (dolist (info *node-infos*)
        (when (= num (me:num info))
          (return (me:short-name (me:user info)))))))

(defun name-to-node (name)
  (dolist (info *node-infos*)
    (when (string= name (me:short-name (me:user info)))
      (return (me:num info)))))

(defun timestamp-to-hour (&optional (secs (get-universal-time)))
  (multiple-value-bind (_ m h)
      (decode-universal-time secs)
    (format nil "~2,'0D:~2,'0D" h m)))

(defun set-gps-position (node pos)
  (flet ((to-float (i)
           (float (/ i (expt 10 7)))))
    (when (me:latitude-i pos)
      (loc:set-position node (list :lat (to-float (me:latitude-i pos))
                                   :lon (to-float (me:longitude-i pos))
                                   :alt (me:altitude pos)
                                   :time (me:time pos))))))

(let (echo-text echo-receiver)
  (defun process-received ()
    "Walks *RECEIVED* FROM-RADIOs and saves relevant data."
    (setf *received* (nreverse *received*))
    (dolist (struct *received*)
      (cond ((me:from-radio.has-packet struct)
             (let* ((packet (me:from-radio.packet struct))
                    (decoded (me:decoded packet)))
               (when decoded
                 (let ((payload (me:payload decoded)))
                   (case (me:portnum decoded)
                     ;; text-message
                     (:text-message-app
                      (let ((timestamp (get-universal-time))
                            (mid (me:id packet))
                            (text (qfrom-utf8 payload)))
                        (setf msg:*message-id* (max mid msg:*message-id*))
                        (if (x:starts-with ":e" text) ; 'echo'
                            (progn
                              (setf echo-text     (subseq text #.(length ":e"))
                                    echo-receiver (me:from packet))
                              (qsingle-shot 1000 (lambda ()
                                                   (let ((*receiver* echo-receiver))
                                                     (send-message (x:cc "<b>:e</b>" echo-text))))))
                            (progn
                              (when (x:starts-with "<b>:e</b>" text)
                                (setf text (msg:echo-message text (me:from packet) (me:rx-snr packet) (me:rx-rssi packet))))
                              (msg:add-message
                               (list :receiver (my-name)
                                     :sender (node-to-name (me:from packet))
                                     :timestamp timestamp
                                     :hour (timestamp-to-hour timestamp)
                                     :text (add-line-breaks text)
                                     :mid mid))))))
                     ;; for :ack-state (acknowledgement state)
                     (:routing-app
                      (let ((state (me:routing.error-reason
                                    (pr:deserialize-from-bytes 'me:routing payload))))
                        (msg:change-state (case state
                                            (:none
                                             :received)
                                            (t
                                             (qlog "message state changed: ~A" state)
                                             :not-received))
                                          (me:request-id decoded))))
                     ;; GPS location
                     (:position-app
                      (unless (zerop (length payload))
                        (set-gps-position (me:from packet)
                                          (pr:deserialize-from-bytes 'me:position payload)))))))))
            ;; my-info
            ((me:from-radio.has-my-info struct)
             (setf *my-node-info* (me:my-node-num (me:my-info struct))))
            ;; node-info
            ((me:from-radio.has-node-info struct)
             (let ((info (me:node-info struct)))
               (if (eql *my-node-info* (me:num info))
                   (setf *my-node-info* info)
                   (setf *node-infos*
                         (nconc *node-infos* (list info))))
               (x:when-it (me:position info)
                 (set-gps-position (me:num info) x:it))
               (when *schedule-clear*
                 (radios:clear)
                 (group:clear))
               (let ((name (me:short-name (me:user info)))
                     (current (= (me:num info)
                                 (me:num *my-node-info*)))
                     (metrics (me:device-metrics info)))
                 (unless current
                   (group:add-person
                    (list :radio-name name
                          :custom-name (or (app:setting name :custom-name) "")
                          :node-num (me:num info)
                          :current (equal name (app:setting :latest-receiver)))))
                 (when (find name *ble-names* :test 'string=)
                   (setf radios:*found* t)
                   (radios:add-radio
                    (list :name name
                          :hw-model (symbol-name (me:hw-model (me:user info)))
                          :battery-level (float (if metrics (me:battery-level metrics) 0))
                          :current current))
                   (when current
                     (app:update-current-device name))))))
            ;; channel
            ((me:from-radio.has-channel struct)
             (let ((channel (me:channel struct)))
               (when (eql :primary (me:role channel))
                 (setf *my-channel* channel))
               (push channel *channels*)))
            ;; config lora
            ((me:from-radio.has-config struct)
             (let ((config (me:config struct)))
               (when (me:config.has-lora config)
                 (setf *config-lora* (me:lora config)))))
            ;; config-complete-id
            ((me:from-radio.has-config-complete-id struct)
             (when (= *config-id* (me:config-complete-id struct))
               (setf *config-complete* t)
               (q> |playing| ui:*busy* nil)
               (qlog "config-complete id: ~A" *config-id*)
               (when (and *my-channel*
                          (string/= *channel-name*
                                    (me:name (me:settings *my-channel*))))
                 (qlater 'config-device))))))
    (setf *received* nil)))

(defun send-admin (admin-message)
  (send-to-radio
   (to-radio :to (me:num *my-node-info*)
             :id (msg:new-message-id)
             :hop-limit 3
             :want-ack t
             :priority :reliable
             :decoded (me:make-data
                       :portnum :admin-app
                       :payload (pr:serialize-to-bytes admin-message)
                       :want-response t))))

(defun set-channel (channel)
  (send-admin (me:make-admin-message
               :set-channel (setf *my-channel* channel))))

(defun change-lora-config ()
  (send-admin 
   (me:make-admin-message
    :set-config (me:make-config
                 :lora (me:make-config.lo-ra-config
                        :use-preset t
                        :modem-preset (app:setting :modem-preset)
                        :region (app:setting :region)
                        :hop-limit 3
                        :tx-enabled t
                        :tx-power 27
                        :sx126x-rx-boosted-gain t))))
  ;; device will reboot after changing lora config
  (app:toast (tr "waiting for reboot..."))
  (qsleep 5)
  (q> |playing| ui:*busy* t)
  (qsleep 20)
  (start-device-discovery))

(defun change-region (&optional (region "")) ; see QML
  (cond ((x:empty-string region)
         (qlater 'radios:choose-region))
        ((string/= region (app:setting :region))
         (app:change-setting :region (app:kw region))
         (qlater 'change-lora-config)))
  (values))

(defun change-modem-preset (modem-preset) ; see QML
  (app:change-setting :modem-preset (app:kw modem-preset))
  (qlater 'change-lora-config)
  (values))

(defun config-device ()
  "Will be called once for every new device, in order to be able to
  communicate on the same channel."
  ;; channel settings for direct messages
  (set-channel (me:make-channel
                :settings (me:make-channel-settings
                           :name *channel-name*
                           :psk (to-bytes (list 1))) ; encrypted with fixed (known) key
                :role :primary))
  (change-lora-config))

(defun send-position (pos &optional (to (my-num)))
  "Send GPS position to radio."
  (flet ((to-int (f)
           (floor (* f (expt 10 7)))))
    (send-to-radio
     (to-radio :from (my-num)
               :to to
               :hop-limit 3
               :priority :background
               :decoded (me:make-data
                         :portnum :position-app
                         :payload (pr:serialize-to-bytes
                                   (if pos
                                       (me:make-position
                                        :latitude-i (to-int (getf pos :lat))
                                        :longitude-i (to-int (getf pos :lon))
                                        :time (getf pos :time))
                                       (me:make-position))) ; unset
                         :want-response t)))))

(defun channel-to-url (&optional channel)
  (let ((base64 (base64:usb8-array-to-base64-string
                 (pr:serialize-to-bytes (or channel *my-channel*)))))
    ;; remove padding, substitute characters as by definition
    (x:cc "https:/meshtastic.org/e/#"
          (string-right-trim "=" (substitute #\- #\+ (substitute #\_ #\/ base64))))))

(defun url-to-channel (url &optional (set t))
  (let ((base64 (subseq url (+ 2 (search "/#" url)))))
    ;; re-add padding
    (setf base64 (x:cc base64
                       (make-string (mod (length base64) 4) :initial-element #\=)))
    (let ((channel (pr:deserialize-from-bytes
                    (base64:base64-string-to-usb8-array base64))))
      (if set
          (set-channel channel)
          channel))))

(defun change-receiver (receiver) ; see QML
  (setf *receiver* receiver)
  (app:change-setting :latest-receiver (node-to-name *receiver*))
  (msg:receiver-changed)
  (group:receiver-changed)
  (values))

(defun edit-channel-name () ; see QML
  (if *config-complete*
      (app:input-dialog
       (tr "Channel name:") 'channel-name-changed
       :title (tr "Name")
       :text *channel-name*
       :max-length #.(float 12))
      (app:message-dialog (tr "Radio not ready yet."))))

(defun channel-name-changed (ok)
  (when ok
    (let ((name (q< |text| ui:*dialog-line-edit*)))
      (when (and (not (x:empty-string name))
                 (string/= name *channel-name*))
        (setf *channel-name* name)
        (q> |text| ui:*channel-name* *channel-name*)
        (app:change-setting :channel-name name)
        (config-device)))))

(defun edit-device-filter () ; see QML
  (app:input-dialog
   (tr "Device filter:") 'device-filter-changed
   :title (tr "Filter")
   :text (or (app:setting :device-filter) "meshtastic")))

(defun device-filter-changed (ok)
  (when ok
    (let ((name (q< |text| ui:*dialog-line-edit*)))
      (qt:set-device-filter qt:*cpp* name)
      (app:change-setting :device-filter name))))

(defun keywords (name)
  (pr:enum-keywords (ecase name
                      (:modem-preset
                       'me:config.lo-ra-config.modem-preset)
                      (:region-code
                       'me:config.lo-ra-config.region-code))))

