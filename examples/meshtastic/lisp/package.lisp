(defpackage :app
  (:use :cl :qml)
  (:export
   #:change-setting
   #:icon-press-and-hold
   #:in-data-path
   #:ini
   #:load-settings
   #:my-ip
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
   #:reset-default-radio))

(defpackage :location
  (:nicknames :loc)
  (:use :cl :qml)
  (:export
   #:*my-position*
   #:*positions*
   #:activate-map
   #:check-offline-map
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

