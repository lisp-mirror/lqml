(defpackage :app
  (:use :cl :qml)
  (:export
   #:*backup-data-file*
   #:*backup-map-file*
   #:change-setting
   #:confirm-dialog
   #:icon-press-and-hold
   #:in-data-path
   #:ini
   #:load-settings
   #:message-dialog
   #:make-backup
   #:my-ip
   #:restore-eventual-backup
   #:save-settings
   #:setting
   #:toast
   #:kw
   #:unzip
   #:view-index-changed
   #:zip))

(defpackage :lora
  (:use :cl :qml)
  (:local-nicknames (:pr :cl-protobufs)
                    (:me :cl-protobufs.meshtastic))
  (:export
   #:*channel*
   #:*channels*
   #:*config-complete*
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
   #:keywords
   #:my-name
   #:my-num
   #:send-position
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
   #:ini
   #:name-edited
   #:receiver-changed
   #:set-unread))

(defpackage :db
  (:use :cl)
  (:export
   #:delete-message
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
   #:echo-message
   #:find-text
   #:font-size-changed
   #:font-size-dialog
   #:ini
   #:message-id
   #:message-press-and-hold
   #:receiver-changed
   #:show-date
   #:show-messages
   #:swipe-to-left))

(defpackage :radios
  (:use :cl :qml)
  (:export
   #:*found*
   #:add-radio
   #:change-radio
   #:clear
   #:device-discovered
   #:ini
   #:reset-default-radio))

(defpackage :location
  (:nicknames :loc)
  (:use :cl :qml)
  (:export
   #:*my-position*
   #:*positions*
   #:activate-map
   #:distance
   #:extract-map-bin
   #:ini
   #:last-gps-position
   #:make-map-bin
   #:position*
   #:position-count
   #:set-position
   #:tile-path
   #:tile-provider-path
   #:update-my-position))

