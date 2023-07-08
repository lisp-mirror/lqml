(defpackage :app
  (:use :cl :qml)
  (:export
   #:change-setting
   #:icon-press-and-hold
   #:ini
   #:load-settings
   #:save-settings
   #:setting
   #:toast
   #:kw
   #:view-index-changed))

(defpackage :lora
  (:use :cl :qml)
  (:local-nicknames (:pr :cl-protobufs)
                    (:me :cl-protobufs.meshtastic))
  (:export
   #:*channel*
   #:*channels*
   #:*config-lora*
   #:*my-node-info*
   #:*node-infos*
   #:*primary-channel*
   #:*reading*
   #:*ready*
   #:*received*
   #:*receiver*
   #:*remote-node*
   #:*schedule-clear*
   #:*settings*
   #:change-receiver
   #:change-region
   #:change-modem-preset
   #:channel-to-url
   #:ini
   #:start-config
   #:start-device-discovery
   #:read-radio
   #:received-from-radio
   #:region-changed
   #:send-message
   #:send-to-radio
   #:url-to-channel))

(defpackage :group
  (:use :cl :qml)
  (:export
   #:add-person
   #:clear
   #:name-edited
   #:receiver-changed
   #:set-unread))

(defpackage :db
  (:use :cl)
  (:export
   #:ini
   #:load-message
   #:load-messages
   #:max-message-id
   #:save-message
   #:update-message))

(defpackage :messages
  (:nicknames :msg)
  (:use :cl :qml)
  (:export
   #:*message-id*
   #:*states*
   #:add-message
   #:change-state
   #:check-utf8-length
   #:clear-find
   #:find-text
   #:message-press-and-hold
   #:receiver-changed
   #:show-messages))

(defpackage :radios
  (:use :cl :qml)
  (:export
   #:add-radio
   #:change-radio
   #:clear))

