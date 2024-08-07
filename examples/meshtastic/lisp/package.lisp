(defpackage :app
  (:use :cl :qml)
  (:export
   #:*background-mode*
   #:*backup-data-file*
   #:*backup-map-file*
   #:background-mode-changed
   #:change-setting
   #:confirm-dialog
   #:has-feature
   #:icon-press-and-hold
   #:in-data-path
   #:ini
   #:input-dialog
   #:load-settings
   #:message-dialog
   #:make-backup
   #:my-ip
   #:on-config-complete
   #:restore-eventual-backup
   #:save-settings
   #:setting
   #:toast
   #:kw
   #:unzip
   #:update-current-device
   #:view-index-changed
   #:zip))

(defpackage :lora
  (:use :cl :qml)
  (:local-nicknames (:pr :cl-protobufs)
                    (:me :cl-protobufs.meshtastic))
  (:export
   #:+broadcast-id+
   #:*broadcast-name*
   #:*channel*
   #:*channel-name*
   #:*channels*
   #:*config-complete*
   #:*config-lora*
   #:*my-node-info*
   #:*node-infos*
   #:*print-json*
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
   #:edit-channel-name
   #:edit-device-filter
   #:get-node-config
   #:ini
   #:keywords
   #:my-name
   #:my-num
   #:process-saved-packets
   #:radio-ready-p
   #:reset-node-db
   #:send-enqueued
   #:send-position
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
   #:modem-preset
   #:name-edited
   #:receiver-changed
   #:set-modem-preset
   #:set-unread
   #:unread-messages-p))

(defpackage :db
  (:use :cl :qml)
  (:export
   #:delete-message
   #:export-to-list
   #:ini
   #:load-message
   #:load-messages
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
   #:message-press-and-hold
   #:new-message-id
   #:receiver-changed
   #:show-date
   #:show-messages
   #:swipe-to-left))

(defpackage :radios
  (:use :cl :qml)
  (:export
   #:*found*
   #:*connection*
   #:add-radio
   #:change-radio
   #:choose-region
   #:clear
   #:device-discovered
   #:ensure-wifi-connection
   #:ini
   #:reset
   #:saved-region
   #:set-region
   #:wifi-connectable
   #:wifi-ip))

(defpackage :location
  (:nicknames :loc)
  (:use :cl :qml)
  (:export
   #:*positions*
   #:activate-map
   #:add-manual-marker
   #:check-position-update
   #:distance
   #:extract-map-bin
   #:ini
   #:latest-gps-position
   #:make-map-bin
   #:position*
   #:position-count
   #:remove-marker
   #:set-position
   #:share-my-location
   #:tile-path
   #:tile-provider-path
   #:update-my-position))

