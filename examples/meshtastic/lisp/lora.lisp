(in-package :lora)

(defvar *settings* (list :region :eu-868)) ; Europe 868 MHz

(defvar *my-channel*   nil)
(defvar *channels*     nil)
(defvar *my-node-info* nil)
(defvar *node-infos*   nil)
(defvar *config-lora*  nil)

;;; header

(defun lsb (size)
  (ldb (byte 8 0) size))

(defun msb (size)
  (ldb (byte 8 8) size))

(defun header (size)
  (vector #x94 #xc3 (msb size) (lsb size)))

;;; ini/send/receive

(defvar *config-id* 0)
(defvar *notify-id* nil)
(defvar *ready*     nil)
(defvar *reading*   nil)
(defvar *received*  nil)

(defun to-bytes (list)
  (make-array (length list)
              :element-type '(unsigned-byte 8)
              :initial-contents list))

(defun start-device-discovery (&optional (name ""))
  (setf radios:*schedule-clear* t)
  (qt:start-device-discovery qt:*ble* name)
  (q> |playing| ui:*busy* t))

(defun start-config ()
  (when *ready*
    (incf *config-id*)
    (send-to-radio
     (me:make-to-radio :want-config-id *config-id*))))

(defun set-ready (&optional (ready t)) ; called from Qt
  (setf *ready* ready)
  (when ready
    (qlater 'start-config))
  (values))

(defun send-message (text)
  "Sends TEXT to radio and adds it to QML item model."
  (incf msg:*message-id*)
  (send-to-radio
   (me:make-to-radio
    :packet (me:make-mesh-packet
             :from (me:num *my-node-info*)
             :to (me:num (first *node-infos*)) ; assumes just 2 radios (for now)
             :id msg:*message-id*
             :want-ack t
             :decoded (me:make-data
                       :portnum :text-message-app
                       :payload (babel:string-to-octets text)))))
  (msg:add-message
   (list :text text
         :sender (me:short-name (me:user *my-node-info*))
         :me t
         :timestamp (timestamp-to-string)
         :mid msg:*message-id*
         :ack-state (position :not-received msg:*states*))))

(defun read-radio ()
  "Triggers a read on the radio. Will call RECEIVED-FROM-RADIO on success."
  (qrun* (qt:read* qt:*ble*)))

(defun send-to-radio (to-radio)
  "Sends passed TO-RADIO, preceded by a header."
  (pr:print-json to-radio)
  (let ((bytes (pr:serialize-to-bytes to-radio)))
    (qrun*
     (qt:write* qt:*ble* (header (length bytes)))
     (qt:write* qt:*ble* bytes))))

(defun received-from-radio (bytes &optional notified) ; called from Qt
  (if notified
      (progn
        (setf *notify-id* bytes)
        (read-radio))
      (let ((from-radio (pr:deserialize-from-bytes 'me:from-radio bytes)))
        (setf *reading* t)
        (pr:print-json from-radio)
        (push from-radio *received*)))
  (values))

(defun receiving-done () ; called from Qt
  (setf *reading* nil)
  (process-received)
  (values))

(defun node-to-name (num)
  (dolist (info *node-infos*)
    (when (= num (me:num info))
      (return (me:short-name (me:user info))))))

(defun timestamp-to-string (&optional (secs (get-universal-time)))
  (multiple-value-bind (_ m h)
      (decode-universal-time secs)
    (format nil "~D:~2,'0D" h m)))

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
                    (msg:add-message
                     (list :text (babel:octets-to-string payload)
                           :sender (node-to-name (me:from packet))
                           :timestamp (timestamp-to-string))))
                   ;; for :ack-state (acknowledgement state)
                   (:routing-app
                    (msg:change-state (case (me:routing.error-reason
                                             (pr:deserialize-from-bytes 'me:routing payload))
                                        (:none :received))
                                      (me:request-id decoded))))))))
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
             (when radios:*schedule-clear*
               (radios:clear))
             (let ((name (me:short-name (me:user info)))
                   (current (= (me:num info)
                               (me:num *my-node-info*))))
               (radios:add-radio
                (list :name name
                      :hw-model (symbol-name (me:hw-model (me:user info)))
                      :battery-level (me:battery-level (me:device-metrics info))
                      :current current))
               (when current
                 (setf (getf *settings* :device) name))))
                 (app:save-settings))
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
             (qlater 'config-device)
             (q> |playing| ui:*busy* nil)
             (qlog :config-complete *config-id*)))))
  (setf *received* nil))

(defun send-admin (admin-message)
  (send-to-radio
   (me:make-to-radio
    :packet (me:make-mesh-packet
             :id (incf msg:*message-id*)
             :want-ack t
             :decoded (me:make-data
                       :portnum :admin-app
                       :payload (pr:serialize-to-bytes admin-message)
                       :want-response t)))))

(defun set-channel (channel)
  (send-admin (me:make-admin-message
               :set-channel (setf *my-channel* channel))))

(defun config-device ()
  "Absolut minimum necessary for sending text messages."
  ;; lora settings
  (send-admin 
   (me:make-admin-message
    :set-config (me:make-config
                 :lora (me:make-config.lo-ra-config
                        :use-preset t
                        :region (getf *settings* :region)
                        :hop-limit 3
                        :tx-enabled t))))
  ;; channel settings
  (set-channel (me:make-channel
                :settings (me:make-channel-settings :psk (to-bytes (list 1)))
                :role :primary)))

(defun channel-to-url (&optional channel)
  (let ((base64 (base64:usb8-array-to-base64-string
                 (pr:serialize-to-bytes (or channel *my-channel*)))))
    ;; remove padding, substitute characters as by definition
    (x:cc "https:/meshtastic.org/e/#"
          (string-right-trim "=" (substitute #\- #\+ (substitute #\_ #\/ base64))))))

(defun url-to-channel (url &optional (set t))
  (let ((base64 (+ 2 (subseq "/#" url))))
    ;; re-add padding
    (setf base64 (x:cc base64
                       (make-string (mod (length base64) 4) :initial-element #\=)))
    (let ((channel (pr:deserialize-from-bytes
                    (base64:base64-string-to-usb8-array base64))))
      (if set
          (set-channel channel)
          channel))))
